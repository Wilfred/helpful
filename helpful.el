;;; helpful.el --- a better *help* buffer            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: help, lisp
;; Version: 0.1
;; Package-Requires: ((dash "2.12.0") (s "1.11.0") (elisp-refs "1.2"))

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

(require 'elisp-refs)
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
          (-map
           (-lambda ((sym val))
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
     "Forget function"
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

(define-button-type 'helpful-navigate-button
  'action #'helpful--navigate
  'path nil
  'position nil
  'follow-link t
  'help-echo "Navigate to definition")

(defun helpful--navigate (button)
  "Navigate to the path this button represents."
  (find-file (button-get button 'path))
  ;; We use `get-text-property' to work around an Emacs 25 bug:
  ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
  (-when-let (pos (get-text-property button 'position
                                     (marker-buffer button)))
    (goto-char pos)))

(defun helpful--navigate-button (path &optional pos)
  "Return a button that opens PATH and puts point at POS."
  (with-temp-buffer
    (insert-text-button
     (abbreviate-file-name path)
     :type 'helpful-navigate-button
     'path path
     'position pos)
    (buffer-string)))

(defun helpful--syntax-highlight (source)
  "Return a propertized version of elisp SOURCE."
  (with-temp-buffer
    (insert source)
    (delay-mode-hooks (emacs-lisp-mode))
    (font-lock-ensure)
    (buffer-string)))

;; TODO: allow RET to go the relevant line of code.
(defun helpful--source (sym)
  "Return the source code of SYM.
If the source code cannot be found, return the sexp used."
  (-if-let ((buf . start-pos) (helpful--definition sym))
      (with-current-buffer buf
        (save-excursion
          (goto-char start-pos)
          (forward-sexp)
          (buffer-substring-no-properties start-pos (point))))
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
      (-when-let (marker (get sym 'edebug))
        (cons (marker-buffer marker)
              (marker-position marker))))))

(defun helpful--source-path (sym)
  "Return the path where SYM is defined."
  (-when-let ((buf . pos) (helpful--definition sym))
    (buffer-file-name buf)))

(defun helpful--source-pos (sym)
  "Return the file position where SYM is defined."
  (-when-let ((buf . pos) (helpful--definition sym))
    pos))

(defun helpful--reference-positions (sym buf)
  "Return all the buffer positions of references to SYM in BUF."
  (-let* ((forms-and-bufs
           (elisp-refs--search-1
            (list buf)
            (lambda (buf)
              (elisp-refs--read-and-find buf sym #'elisp-refs--function-p))))
          ;; Since we only searched one buffer, we know that
          ;; forms-and-bufs has only one item.
          (forms-and-buf (-first-item forms-and-bufs))
          ((forms . _buf) forms-and-buf))
    (-map
     (-lambda ((_code start-pos _end-pos)) start-pos)
     forms)))

(defun helpful--all-keymap-syms ()
  "Return all keymaps defined in this Emacs instance."
  (let (keymaps)
    (mapatoms
     (lambda (sym)
       (when (and
              (boundp sym)
              (keymapp (symbol-value sym))
              (s-ends-with-p "-mode-map" (symbol-name sym)))
         (push sym keymaps))))
    keymaps))

(defun helpful--keymaps-containing (command-sym)
  (let (matching-keymaps)
    ;; Look for this command in all majro and minor mode maps.
    (dolist (keymap (helpful--all-keymap-syms))
      (let ((keycodes (where-is-internal command-sym
                                         (list (symbol-value keymap)))))
        (when keycodes
          (push (cons keymap
                      (-map #'key-description keycodes))
                matching-keymaps))))
    ;; Look for this command in the global map.
    (let ((keycodes (where-is-internal command-sym
                                       (list (current-global-map)))))
      (when keycodes
        (push (cons 'global
                    (-map #'key-description keycodes))
              matching-keymaps)))
    matching-keymaps))

(defun helpful--format-keys (command-sym)
  "Describe all the keys that call COMMAND-SYM.
Ensures global keybindings are shown first."
  (let (mode-lines
        global-lines)
    (--each (helpful--keymaps-containing command-sym)
      (-let [(map . keys) it]
        (dolist (key keys)
          (push
           (format "%s %s"
                   (propertize (symbol-name map) 'face 'font-lock-variable-name-face)
                   key)
           (if (eq map 'global) global-lines mode-lines)))))
    (setq global-lines (-sort #'string< global-lines))
    (setq mode-lines (-sort #'string< mode-lines))
    (-let [lines (-concat global-lines mode-lines)]
      (if lines
          (s-join "\n" lines)
        "This command is not in any keymaps."))))

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
          (-map (-lambda ((def name))
                  (helpful--syntax-highlight
                   (format "(%s %s ...)" def name)))
                position-heads)))

(defun helpful-update ()
  "Update the current *Helpful* buffer to the latest
state of the current symbol."
  (interactive)
  (cl-assert (not (null helpful--sym)))
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
              (-uniq (--map (helpful--position-head buf it) positions)))
        (kill-buffer buf)))
    (erase-buffer)
    (insert
     (helpful--heading "Signature\n")
     (helpful--signature helpful--sym)
     (helpful--heading "\n\nDocumentation\n")
     ;; TODO: a link to find this symbol in the manual, much like
     ;; helpfns+ or counsel-info-lookup-symbol.
     (or (helpful--docstring helpful--sym)
         "No docstring."))
    (when (commandp helpful--sym)
      (insert
       (helpful--heading "\n\nKey Bindings\n")
       (helpful--format-keys helpful--sym)))
    (insert
     (helpful--heading "\n\nSymbol Properties\n")
     (or (helpful--format-properties helpful--sym)
         "No properties.")
     (helpful--heading "\n\nReferences\n")
     (if source-path
         (format "Defined in %s\n\nCallers:\n%s\n"
                 (helpful--navigate-button
                  source-path
                  (helpful--source-pos helpful--sym))
                 (helpful--format-position-heads references))
       "Could not find source file.\n")
     (helpful--heading "\n\nDefinition\n")
     (if (stringp source)
         (helpful--syntax-highlight source)
       (helpful--pretty-print source))
     "\n\n"
     (helpful--disassemble-button)
     " "
     (helpful--forget-button))
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
                           #'fboundp nil nil nil
                           (when (fboundp sym-here)
                             (symbol-name sym-here))))))

(defun helpful (symbol)
  "Show Help for SYMBOL."
  (interactive
   (list (helpful--read-fn-symbol)))
  (switch-to-buffer (helpful--buffer symbol))
  (helpful-update))

(define-derived-mode helpful-mode special-mode "Helpful"
  "Major mode for *Helpful* buffers."
  (add-hook 'xref-backend-functions #'elisp--xref-backend nil t))

(define-key helpful-mode-map (kbd "g") #'helpful-update)

(provide 'helpful)
;;; helpful.el ends here
