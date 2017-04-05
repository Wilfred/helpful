;;; helpful.el --- a better *help* buffer            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: help, lisp
;; Version: 0.1
;; Package-Requires: ((dash "2.12.0") (s "1.11.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'help)
(require 'dash)
(require 's)

(defvar-local helpful--sym nil)

(defun helpful--buffer (symbol)
  "Return a buffer to show help for SYMBOL in."
  (let ((buf (get-buffer-create
              (format "*helpful: %s*" symbol))))
    (with-current-buffer buf
      (helpful-mode)
      (setq helpful--sym symbol))
    buf))

(defun helpful--heading (text)
  "Propertize TEXT as a heading."
  (propertize text 'face 'bold))

(defun helpful--pretty-print (val)
  "Pretty-print VALUE.
This allows us to distinguish strings from symbols."
  (with-temp-buffer
    (cl-prettyprint val)
    (s-trim (buffer-string))))

(defun helpful--format-properties (symbol)
  "Return a string describing all the properties of SYMBOL."
  (let* ((syms-and-vals
          (-partition 2 (symbol-plist symbol)))
         (lines
          (--map
           (-let [(sym val) it]
             (format "%s %s"
                     (propertize (symbol-name sym)
                                 'face 'font-lock-constant-face)
                     (helpful--pretty-print val)))
           syms-and-vals)))
    (when lines
      (s-join "\n" lines))))

(define-button-type 'helpful-forget-button
  'action #'helpful--forget
  'follow-link t
  'help-echo "Unbind this function")

;; TODO: it would be nice to optionally delete the source code too.
(defun helpful--forget (_button)
  "Unbind the current symbol."
  (when (functionp helpful--sym)
    (fmakunbound helpful--sym))
  (makunbound helpful--sym)
  (message "Forgot function %s" helpful--sym)
  (kill-buffer (current-buffer)))

(defun helpful--forget-button ()
  "Return a button that unbinds the current symbol"
  (with-temp-buffer
    (insert-text-button
     "Forget"
     :type 'helpful-forget-button)
    (buffer-string)))

(define-button-type 'helpful-disassemble-button
  'action #'helpful--disassemble
  'follow-link t
  'help-echo "Show disassembled bytecode")

(defun helpful--disassemble (_button)
  "Disassemble the current symbol."
  (disassemble helpful--sym))

(defun helpful--disassemble-button ()
  "Return a button that disassembles the current symbol."
  (with-temp-buffer
    (insert-text-button
     "Disassemble bytecode"
     :type 'helpful-disassemble-button)
    (buffer-string)))

;; TODO: consider advising eval-buffer to add the current directory to
;; load-path.
;; TODO: looks like we need to byte-compile the file too, load-path
;; isn't sufficient.
(defun helpful--source (sym)
  "Return the source code of SYM.
If the source code cannot be found, return the sexp used."
  (-if-let ((buf . start-pos) (helpful--definition sym))
      (with-current-buffer buf
        (save-excursion
          (goto-char start-pos)
          (forward-sexp)
          (buffer-substring start-pos (point))))
    ;; Could not find source -- probably defined interactively, or via
    ;; a macro, or file has changed, or a primitive.
    ;; TODO: offer to download C sources for current version.
    (indirect-function sym)))

(defun helpful--definition (sym)
  "Return a pair (BUF . POS) where SYM is defined."
  (let (buf-and-pos)
    (ignore-errors
      (setq buf-and-pos
            (find-function-noselect sym)))
    (if buf-and-pos
        buf-and-pos
      ;; If it's defined interactively, it may have an edebug property
      ;; that tells us where it's defined.
      (-when-let (marker
                  (get #'helpful--format-position-heads 'edebug))
        (cons (marker-buffer marker)
              (marker-position marker))))))

(defun helpful--source-path (sym)
  "Return the path where SYM is defined."
  (-when-let (buf-and-pos (helpful--definition sym))
    (buffer-file-name (car buf-and-pos))))

(defun helpful--reference-positions (sym buf)
  (-let* ((forms-and-bufs
           (elisp-refs--search-1
            (list buf)
            (lambda (buf)
              (elisp-refs--read-and-find buf sym #'elisp-refs--function-p))))
          ;; Since we only searched one buffer, we know that
          ;; forms-and-bufs has only one item.
          (forms-and-buf (-first-item forms-and-bufs))
          ((forms . _buf) forms-and-buf))
    (--map
     (-let [(code start-pos end-pos) it]
       start-pos)
     forms)))

(defun helpful--position-head (buf pos)
  "Find position POS in BUF, and return the name of the outer sexp."
  (with-current-buffer buf
    (goto-char pos)
    (let (finished)
      (while (not finished)
        (condition-case _err
            (backward-up-list)
          (error (setq finished t))))
      (-take 2 (read buf)))))

(defun helpful--format-position-heads (position-heads)
  (s-join "\n"
          (--map (-let [(def name) it]
                   (format "(%s %s ...)"
                           (propertize (symbol-name def) 'face 'font-lock-keyword-face)
                           (propertize (symbol-name name) 'face 'font-lock-function-name-face)))
                 position-heads)))

(defun helpful-update ()
  "Update the current *Helpful* buffer to the latest
state of the current symbol."
  (interactive)
  (let ((inhibit-read-only t)
        (start-pos (point))
        (source (helpful--source helpful--sym))
        (source-path (helpful--source-path helpful--sym))
        references)
    (when source-path
      (let* ((buf (elisp-refs--contents-buffer source-path))
             (positions
              (helpful--reference-positions helpful--sym buf)))
        (setq references
              (--map (helpful--position-head buf it) positions))))
    (erase-buffer)
    (insert
     (helpful--heading "Signature\n")
     (helpful--signature helpful--sym)
     (helpful--heading "\n\nDocumentation\n")
     (or (helpful--docstring helpful--sym)
         "No docstring.")
     (helpful--heading "\n\nSymbol Properties\n")
     (or (helpful--format-properties helpful--sym)
         "No properties.")
     (helpful--heading "\n\nReferences\n")
     (if source-path
         (format "Defined in %s\n%s\n"
                 source-path
                 (helpful--format-position-heads references))
       "Could not find source file.\n")
     (helpful--heading "\nTools\n")
     (helpful--forget-button)
     (helpful--heading "\n\nDefinition\n")
     (if (stringp source)
         source
       (helpful--pretty-print source))
     "\n\n"
     (helpful--disassemble-button))
    (goto-char start-pos)))

(defun helpful--skip-advice (docstring)
  "Remove mentions of advice from DOCSTRING."
  (let* ((lines (s-lines docstring))
         (relevant-lines
          (--drop-while (s-starts-with-p ":around advice:" it) lines)))
    (s-trim (s-join "\n" relevant-lines))))

(defun helpful--format-argument (arg)
  "Format ARG (a symbol) according to Emacs help conventions."
  (let ((arg-str (symbol-name arg)))
    (if (s-starts-with-p "&" arg-str)
        arg-str
      (s-upcase arg-str))))

(defun helpful--signature (sym)
  "Get the signature for function SYM, as a string.
For example, \"(some-func FOO &optional BAR)\"."
  (let (docstring-sig
        source-sig)
    ;; Get the usage from the function definition.
    (let ((formatted-args
           (-map #'helpful--format-argument
                 (help-function-arglist sym))))
      (setq source-sig
            (if formatted-args
                (format "(%s %s)" sym
                        (s-join " " formatted-args))
              (format "(%s)" sym))))
    
    ;; If the docstring ends with (fn FOO BAR), extract that.
    (-when-let (docstring (documentation sym))
      (-when-let (docstring-with-usage (help-split-fundoc docstring sym))
        (setq docstring-sig (car docstring-with-usage))))
    
    (or docstring-sig source-sig)))

;; TODO: propertize arguments and add links to `foo' and Info mentions.
;; TODO: add button for searching the manual.
(defun helpful--docstring (sym)
  "Get the docstring for SYM."
  (-when-let (docstring (documentation sym))
    (-when-let (docstring-with-usage (help-split-fundoc docstring sym))
      (setq docstring (cdr docstring-with-usage))
      (when docstring
        ;; Advice mutates the docstring, see
        ;; `advice--make-docstring'. Undo that.
        ;; TODO: Only do this if the function is advised.
        (setq docstring (helpful--skip-advice docstring))))
    docstring))

(defun helpful--read-fn-symbol ()
  (let ((sym-here (symbol-at-point)))
    (read (completing-read "Symbol: " obarray
                           nil nil nil nil
                           (when (fboundp sym-here)
                             (symbol-name sym-here))))))

(defun helpful (symbol)
  "Show Help for SYMBOL."
  (interactive
   (list (helpful--read-fn-symbol)))
  (switch-to-buffer (helpful--buffer symbol))
  (helpful-update))

(define-derived-mode helpful-mode special-mode "Helpful")

(define-key helpful-mode-map (kbd "g") #'helpful-update)


(provide 'helpful)
;;; helpful.el ends here
