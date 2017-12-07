;;; helpful.el --- a better *help* buffer            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; URL: https://github.com/Wilfred/helpful
;; Keywords: help, lisp
;; Version: 0.4
;; Package-Requires: ((emacs "24.4") (dash "2.12.0") (s "1.11.0") (elisp-refs "1.2") (shut-up "0.3"))

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

;; Helpful is a replacement for *help* buffers that provides much more
;; contextual information.  To get started, try:
;; `M-x helpful-function RET helpful-function
;;
;; The full set of commands you can try is:
;;
;; * helpful-function
;; * helpful-command
;; * helpful-key
;; * helpful-macro
;; * helpful-callable
;; * helpful-variable
;; * helpful-at-point
;;
;; For more information and screenshots, see
;; https://github.com/wilfred/helpful

;;; Code:

(require 'elisp-refs)
(require 'help)
(require 'dash)
(require 's)
(require 'shut-up)
(require 'find-func)
(require 'nadvice)
(require 'info-look)

(defvar-local helpful--sym nil)
(defvar-local helpful--callable-p nil)
(defvar-local helpful--associated-buffer nil
  "We store a reference to the buffer we were called from, so we can
show the value of buffer-local variables.")

(defun helpful--kind-name (symbol callable-p)
  "Describe what kind of symbol this is."
  (cond
   ((not callable-p) "variable")
   ((commandp symbol) "command")
   ((macrop symbol) "macro")
   ((functionp symbol) "function")))

(defun helpful--buffer (symbol callable-p)
  "Return a buffer to show help for SYMBOL in."
  (let ((current-buffer (current-buffer))
        (buf (get-buffer-create
              (format "*helpful %s: %s*"
                      (helpful--kind-name symbol callable-p)
                      symbol))))
    (with-current-buffer buf
      (helpful-mode)
      (setq helpful--sym symbol)
      (setq helpful--callable-p callable-p)
      (setq helpful--associated-buffer current-buffer))
    buf))

(defun helpful--heading (text)
  "Propertize TEXT as a heading."
  (propertize text 'face 'bold))

(defun helpful--format-closure (sym form)
  "Given a closure, return an equivalent defun form."
  (-let (((_keyword _env args . body) form)
         (docstring nil))
    (when (stringp (car body))
      (setq docstring (car body))
      (setq body (cdr body))
      ;; Ensure that the docstring doesn't have lines starting with (,
      ;; or it breaks indentation.
      (setq docstring
            (s-replace "\n(" "\n\\(" docstring)))
    (if docstring
        `(defun ,sym ,args ,docstring ,@body)
      `(defun ,sym ,args ,@body))))

(defun helpful--pretty-print (value)
  "Pretty-print VALUE.
This allows us to distinguish strings from symbols."
  (with-temp-buffer
    (cl-prettyprint value)
    (s-trim (buffer-string))))

(defun helpful--format-properties (symbol)
  "Return a string describing all the properties of SYMBOL."
  (let* ((syms-and-vals
          (-partition 2 (symbol-plist symbol)))
         (syms-and-vals
          (-sort (-lambda ((sym1 _) (sym2 _))
                   (string-lessp (symbol-name sym1) (symbol-name sym2)))
                 syms-and-vals))
         (lines
          (-map
           (-lambda ((sym val))
             (format "%s %s"
                     (propertize (symbol-name sym)
                                 'face 'font-lock-constant-face)
                     (if (eq (type-of val) 'compiled-function)
                         "#<compiled-function>"
                       (helpful--pretty-print val))))
           syms-and-vals)))
    (when lines
      (s-join "\n" lines))))

(define-button-type 'helpful-forget-button
  'action #'helpful--forget
  'symbol nil
  'callable-p nil
  'follow-link t
  'help-echo "Unbind this function")

;; TODO: it would be nice to optionally delete the source code too.
(defun helpful--forget (button)
  "Unbind the current symbol."
  (let* ((sym (button-get button 'symbol))
         (callable-p (button-get button 'callable-p))
         (kind (cond
                ((not callable-p) "variable")
                ((functionp sym) "function")
                (t "macro"))))
    (when (yes-or-no-p (format "Forget %s %s?" kind sym))
      (if callable-p
          (fmakunbound sym)
        (makunbound sym))
      (message "Forgot %s %s." kind sym)
      (kill-buffer (current-buffer)))))

(defun helpful--forget-button (symbol callable-p)
  "Return a button that unbinds SYMBOL."
  (with-temp-buffer
    (insert-text-button
     "Forget"
     'symbol symbol
     'callable-p callable-p
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
     "Disassemble"
     :type 'helpful-disassemble-button)
    (buffer-string)))

(define-button-type 'helpful-navigate-button
  'action #'helpful--navigate
  'path nil
  'position nil
  'follow-link t
  'help-echo "Navigate to definition")

(defun helpful--navigate (button)
  "Navigate to the path this BUTTON represents."
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

(define-button-type 'helpful-all-references-button
  'action #'helpful--all-references
  'symbol nil
  'callable-p nil
  'follow-link t
  'help-echo "Find all references to this symbol")

(defun helpful--all-references (button)
  "Find all the references to the symbol that this BUTTON represents."
  (let ((sym (button-get button 'symbol))
        (callable-p (button-get button 'callable-p)))
    (cond
     ((not callable-p)
      (elisp-refs-variable sym))
     ((functionp sym)
      (elisp-refs-function sym))
     ((macrop sym)
      (elisp-refs-macro sym)))))

(defun helpful--all-references-button (sym callable-p)
  "Return a button that finds all references to SYM."
  (with-temp-buffer
    (insert-text-button
     "All references"
     :type 'helpful-all-references-button
     'symbol sym
     'callable-p callable-p)
    (buffer-string)))

(define-button-type 'helpful-manual-button
  'action #'helpful--manual
  'symbol nil
  'follow-link t
  'help-echo "Describe this symbol")

(defun helpful--manual-button (sym)
  "Return a button that shows SYM in the Info manul."
  (with-temp-buffer
    (insert-text-button
     "View in manual"
     :type 'helpful-manual-button
     'symbol sym)
    (buffer-string)))

(defun helpful--manual (button)
  "Open the manual for the system that this BUTTON represents."
  (let ((sym (button-get button 'symbol)))
    (info-lookup 'symbol sym #'emacs-lisp-mode)))

(define-button-type 'helpful-describe-button
  'action #'helpful--describe
  'symbol nil
  'follow-link t
  'help-echo "Describe this symbol")

(defun helpful--describe (button)
  "Describe the symbol that this BUTTON represents."
  (let ((sym (button-get button 'symbol)))
    (helpful-symbol sym)))

(defun helpful--describe-button (sym)
  "Return a button that describes SYM."
  (with-temp-buffer
    (insert-text-button
     (symbol-name sym)
     :type 'helpful-describe-button
     'symbol sym)
    (buffer-string)))

(defun helpful--split-first-line (docstring)
  "If the first line is a standalone sentence, ensure we have a
blank line afterwards."
  (let* ((lines (s-lines docstring))
         (first-line (-first-item lines))
         (second-line (when (> (length lines) 1) (nth 1 lines))))
    (if (and (s-ends-with-p "." first-line)
             (stringp second-line)
             (not (equal second-line "")))
        (s-join "\n"
                (-cons* first-line "" (cdr lines)))
      docstring)))

;; TODO: fix upstream Emacs bug that means `-map' is not highlighted
;; in the docstring for `--map'.
(defun helpful--format-docstring (docstring)
  "Replace cross-references with links in DOCSTRING."
  (s-trim
   (replace-regexp-in-string
    (rx "`" symbol-start (+? anything) symbol-end "'")
    (lambda (it)
      (let* ((sym-name
              (s-chop-prefix "`" (s-chop-suffix "'" it)))
             (sym (intern sym-name)))
        (if (or (boundp sym) (fboundp sym))
            (helpful--describe-button (read sym-name))
          (propertize sym-name
                      'face 'font-lock-constant-face))))
    (helpful--split-first-line docstring)
    t t)))

(defconst helpful--highlighting-funcs
  '(ert--activate-font-lock-keywords
    highlight-quoted-mode
    rainbow-delimiters-mode)
  "Highlighting functions that are safe to run in a temporary buffer.
This is used in `helpful--syntax-highlight' to support extra
highlighting that the user may have configured in their mode
hooks.")

(defun helpful--syntax-highlight (source &optional mode)
  "Return a propertized version of SOURCE in MODE."
  (unless mode
    (setq mode #'emacs-lisp-mode))
  (with-temp-buffer
    (insert source)

    ;; Switch to major-mode MODE, but don't run any hooks.
    (delay-mode-hooks (funcall mode))

    ;; `delayed-mode-hooks' contains mode hooks like
    ;; `emacs-lisp-mode-hook'. Build a list of functions that are run
    ;; when the mode hooks run.
    (let (hook-funcs)
      (dolist (hook delayed-mode-hooks)
        (let ((funcs (symbol-value hook)))
          (setq hook-funcs (append hook-funcs funcs))))

      ;; Filter hooks to those that relate to highlighting, and run them.
      (setq hook-funcs (-intersection hook-funcs helpful--highlighting-funcs))
      (-map #'funcall hook-funcs))

    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings
        (font-lock-fontify-buffer)))
    (buffer-string)))

(defun helpful--source (sym callable-p)
  "Return the source code of SYM.
If the source code cannot be found, return the sexp used."
  (-let ((initial-buffers (buffer-list))
         ((buf . start-pos) (helpful--definition sym callable-p)))
    (if (and buf start-pos)
        (let (source)
          (with-current-buffer buf
            (save-excursion
              (save-restriction
                (goto-char start-pos)
                (narrow-to-defun)
                (setq source (buffer-substring-no-properties (point-min) (point-max))))))
          ;; If we've just created this buffer, close it.
          (unless (-contains-p initial-buffers buf)
            (kill-buffer buf))
          (when (and source (buffer-file-name buf))
            (setq source (propertize source
                                     'helpful-path (buffer-file-name buf)
                                     'helpful-pos start-pos
                                     'helpful-pos-is-start t)))
          source)
      ;; Could not find source -- probably defined interactively, or via
      ;; a macro, or file has changed.
      ;; TODO: verify that the source hasn't changed before showing.
      ;; TODO: offer to download C sources for current version.
      (indirect-function sym))))

(defun helpful--in-manual-p (sym)
  "Return non-nil if SYM is in an Info manual."
  (let ((completions
         (shut-up
           (info-lookup->completions 'symbol 'emacs-lisp-mode))))
    (-when-let (buf (get-buffer " temp-info-look"))
      (kill-buffer buf))
    (or (assoc sym completions)
        (assoc-string sym completions))))

(defun helpful--definition (sym callable-p)
  "Return a pair (BUF . POS) where SYM is defined.

BUF may be an existing buffer or created. Caller is responsible
for cleaning up."
  (let (buf-and-pos)
    (when callable-p
      (ignore-errors
        (setq buf-and-pos
              ;; TODO: if SYM is in a buffer that's already open, this
              ;; moves point.
              (find-function-noselect sym)))
      (unless buf-and-pos
        ;; If it's defined interactively, it may have an edebug property
        ;; that tells us where it's defined.
        (-when-let (marker (get sym 'edebug))
          (setq buf-and-pos
                (cons (marker-buffer marker)
                      (marker-position marker))))))
    (when (not callable-p)
      (condition-case _err
          (setq buf-and-pos (find-definition-noselect sym 'defvar))
        (search-failed nil)))
    buf-and-pos))

(defun helpful--source-path (sym callable-p)
  "Return the path where SYM is defined."
  (-let* ((initial-buffers (buffer-list))
          ((buf . _) (helpful--definition sym callable-p))
          (path (when buf (buffer-file-name buf))))
    ;; If we've just created this buffer, close it.
    (unless (-contains-p initial-buffers buf)
      (kill-buffer buf))
    path))

(defun helpful--source-pos (sym callable-p)
  "Return the file position where SYM is defined."
  (-let ((initial-buffers (buffer-list))
         ((buf . pos) (helpful--definition sym callable-p)))
    ;; If we've just created this buffer, close it.
    (unless (-contains-p initial-buffers buf)
      (kill-buffer buf))
    pos))

(defun helpful--reference-positions (sym callable-p buf)
  "Return all the buffer positions of references to SYM in BUF."
  (-let* ((forms-and-bufs
           (elisp-refs--search-1
            (list buf)
            (lambda (buf)
              (elisp-refs--read-and-find
               buf sym
               (if callable-p
                   #'elisp-refs--function-p
                 #'elisp-refs--variable-p)))))
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
    ;; Look for this command in all major and minor mode maps.
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

(defun helpful--outer-sexp (buf pos)
  "Find position POS in BUF, and return the name of the outer sexp,
along with its position."
  (with-current-buffer buf
    (goto-char pos)
    (beginning-of-defun)
    (list (point) (-take 2 (read buf)))))

(defun helpful--count-values (items)
  "Return an alist of the count of each value in ITEMS.
E.g. (x x y z y) -> ((x . 2) (y . 2) (z . 1))"
  (let (counts)
    (dolist (item items (nreverse counts))
      (-if-let (item-and-count (assoc item counts))
          (setcdr item-and-count (1+ (cdr item-and-count)))
        (push (cons item 1) counts)))))

(defun helpful--advised-p (sym)
  "A list of advice associated with SYM."
  (advice--p (advice--symbol-function sym)))

(defun helpful--format-head (head)
  "Given a 'head' (the first two symbols of a sexp) format and
syntax highlight it."
  (-let* (((def name) head)
          (formatted-name
           (if (and (consp name) (eq (car name) 'quote))
               (format "'%S" (cadr name))
             (format "%S" name)))
          (formatted-def
           (format "(%s %s ...)" def formatted-name))
          )
    (helpful--syntax-highlight formatted-def)))

(defun helpful--format-reference (head longest-head ref-count position path)
  "Return a syntax-highlighted version of HEAD, with a link
to its source location."
  (let ((formatted-count
         (format "%d reference%s"
                 ref-count (if (> ref-count 1) "s" ""))))
    (propertize
     (format
      "%s %s"
      (s-pad-right longest-head " " (helpful--format-head head))
      (propertize formatted-count 'face 'font-lock-comment-face))
     'helpful-path path
     'helpful-pos position)))

(defun helpful--format-position-heads (position-heads path)
  "Given a list of outer sexps, format them for display.
POSITION-HEADS takes the form ((123 (defun foo)) (456 (defun bar)))."
  (let ((longest-head
         (->> position-heads
              (-map (-lambda ((_pos head)) (helpful--format-head head)))
              (-map #'length)
              (-max))))
    (->> (helpful--count-values position-heads)
         (-map (-lambda (((pos head) . count))
                 (helpful--format-reference head longest-head count pos path)))
         (s-join "\n"))))

(defun helpful--primitive-p (sym callable-p)
  "Return t if SYM is defined in C."
  (if callable-p
      (subrp (indirect-function sym))
    (let ((filename (find-lisp-object-file-name sym 'defvar)))
      (or (eq filename 'C-source)
          (and (stringp filename)
               (equal (file-name-extension filename) "c"))))))

(defun helpful-update ()
  "Update the current *Helpful* buffer to the latest
state of the current symbol."
  (interactive)
  (cl-assert (not (null helpful--sym)))
  (let* ((inhibit-read-only t)
         (start-pos (point))
         (primitive-p (helpful--primitive-p
                       helpful--sym helpful--callable-p))
         (look-for-src (or (not primitive-p)
                           find-function-C-source-directory))
         (source (when look-for-src
                   (helpful--source helpful--sym helpful--callable-p)))
         (source-path (when look-for-src
                        (helpful--source-path helpful--sym helpful--callable-p)))
         references)
    (when source-path
      (let* ((buf (elisp-refs--contents-buffer source-path))
             (positions
              (if primitive-p
                  nil
                (helpful--reference-positions
                 helpful--sym helpful--callable-p buf))))
        (setq references
              (--map (helpful--outer-sexp buf it) positions))
        (kill-buffer buf)))
    (erase-buffer)
    (when helpful--callable-p
      (insert
       (helpful--heading
        (if (macrop helpful--sym)
            "Macro Signature\n"
          "Function Signature\n"))
       (helpful--syntax-highlight (helpful--signature helpful--sym))))

    (-when-let (docstring (helpful--docstring
                           helpful--sym helpful--callable-p))
      (when helpful--callable-p
        (insert "\n\n"))
      (insert
       (helpful--heading
        (cond
         ((not helpful--callable-p)
          "Variable Documentation\n")
         ((macrop helpful--sym)
          "Macro Documentation\n")
         (t
          "Function Documentation\n")))
       (helpful--format-docstring docstring))
      (when (helpful--in-manual-p helpful--sym)
        (insert
         "\n\n"
         (helpful--manual-button helpful--sym))))

    (when (not helpful--callable-p)
      (insert
       (helpful--heading "\n\nValue\n")
       (let ((sym helpful--sym)
             (buf (or helpful--associated-buffer (current-buffer))))
         (helpful--pretty-print
          (with-current-buffer buf
            (symbol-value sym))))))

    ;; Show keybindings.
    ;; TODO: allow users to conveniently add and remove keybindings.
    (when (commandp helpful--sym)
      (insert
       (helpful--heading "\n\nKey Bindings\n")
       (helpful--format-keys helpful--sym)))

    (insert
     (helpful--heading "\n\nReferences\n")
     (cond
      ((and source-path references)
       (format "References in %s:\n%s"
               (helpful--navigate-button source-path 0)
               (helpful--format-position-heads references source-path)))
      (source-path
       (format "No references found in %s."
               (helpful--navigate-button source-path 0)))
      ((and primitive-p (null find-function-C-source-directory))
       "C code is not yet loaded.")
      (t
       "Could not find source file."))
     "\n\n"
     (helpful--all-references-button helpful--sym helpful--callable-p))

    (-when-let (formatted-props (helpful--format-properties helpful--sym))
      (insert
       (helpful--heading "\n\nSymbol Properties\n")
       formatted-props))

    (when (helpful--advised-p helpful--sym)
      (insert
       (helpful--heading "\n\nAdvice\n")
       (format
        "This %s is advised." (if (macrop helpful--sym) "macro" "function"))))

    (insert
     (helpful--heading "\n\nDebugging\n")
     (if (or (not helpful--callable-p) primitive-p)
         ""
       (concat
        (helpful--disassemble-button)
        " "))
     (helpful--forget-button helpful--sym helpful--callable-p)

     (helpful--heading "\n\nSource Code\n")
     (cond
      (source-path
       (concat
        (propertize
         (if primitive-p "// Defined in " ";; Defined in ")
         'face 'font-lock-comment-face)
        (helpful--navigate-button
         source-path
         (helpful--source-pos helpful--sym helpful--callable-p))
        "\n"))
      (primitive-p
       (propertize
        "C code is not yet loaded."
        'face 'font-lock-comment-face))
      (t
       (helpful--syntax-highlight
        (format ";; Source file is unknown\n")))))
    (when source
      (insert
       (cond
        ((stringp source)
         (helpful--syntax-highlight source (if primitive-p 'c-mode)))
        ((and (consp source) (eq (car source) 'closure))
         (helpful--syntax-highlight
          (concat ";; Closure converted to defun by helpful.\n"
                  (helpful--pretty-print
                   (helpful--format-closure helpful--sym source)))))
        (t
         (helpful--syntax-highlight
          (helpful--pretty-print source))))))
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
    (let* ((function-args (help-function-arglist sym))
           (formatted-args
            (if (listp function-args)
                (-map #'helpful--format-argument
                      function-args)
              (list function-args))))
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

;; TODO: Info mentions, e.g. `define-derived-mode' or `defface'.
(defun helpful--docstring (sym callable-p)
  "Get the docstring for SYM."
  (let ((text-quoting-style 'grave)
        docstring)
    (if callable-p
        (progn
          (setq docstring (documentation sym))
          (-when-let (docstring-with-usage (help-split-fundoc docstring sym))
            (setq docstring (cdr docstring-with-usage))
            (when docstring
              ;; Advice mutates the docstring, see
              ;; `advice--make-docstring'. Undo that.
              ;; TODO: Only do this if the function is advised.
              (setq docstring (helpful--skip-advice docstring)))))
      (setq docstring
            (documentation-property sym 'variable-documentation)))
    docstring))

;; TODO: allow jumping to source by pressing RET on the inline code.

(defun helpful--read-symbol (prompt predicate)
  (let ((sym-here (symbol-at-point)))
    (read (completing-read prompt obarray
                           predicate t nil nil
                           (when (funcall predicate sym-here)
                             (symbol-name sym-here))))))

;;;###autoload
(defun helpful-function (symbol)
  "Show help for function named SYMBOL."
  (interactive
   (list (helpful--read-symbol "Function: " #'functionp)))
  (pop-to-buffer (helpful--buffer symbol t))
  (helpful-update))

;;;###autoload
(defun helpful-command (symbol)
  "Show help for interactive function named SYMBOL."
  (interactive
   (list (helpful--read-symbol "Command: " #'commandp)))
  (pop-to-buffer (helpful--buffer symbol t))
  (helpful-update))

;;;###autoload
(defun helpful-key (key-sequence)
  "Show help for interactive command bound to KEY-SEQUENCE."
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence)))
    (unless sym
      (user-error "No command is bound to %s"
                  (key-description key-sequence)))
    (unless (commandp sym)
      (user-error "%s is bound to symbol %s which is not a command"
                  (key-description key-sequence)
                  sym))
    (pop-to-buffer (helpful--buffer sym t))
    (helpful-update)))

;;;###autoload
(defun helpful-macro (symbol)
  "Show help for macro named SYMBOL."
  (interactive
   (list (helpful--read-symbol "Macro: " #'macrop)))
  (pop-to-buffer (helpful--buffer symbol t))
  (helpful-update))

;;;###autoload
(defun helpful-callable (symbol)
  "Show help for function or macro named SYMBOL.

See also `helpful-macro' and `helpful-function'."
  (interactive
   (list (helpful--read-symbol "Function/macro: " #'fboundp)))
  (pop-to-buffer (helpful--buffer symbol t))
  (helpful-update))

(defun helpful--variable-p (symbol)
  "Return non-nil if SYMBOL is a variable."
  (or (get symbol 'variable-documentation)
      (and (boundp symbol)
           (not (keywordp symbol))
           (not (eq symbol nil))
           (not (eq symbol t)))))

(defun helpful--bound-p (symbol)
  "Return non-nil if SYMBOL is a variable or callable.

This differs from `boundp' because we do not consider nil, t
or :foo."
  (or (fboundp symbol)
      (helpful--variable-p symbol)))

;;;###autoload
(defun helpful-symbol (symbol)
  "Show help for SYMBOL, a variable, function or macro.

See also `helpful-callable' and `helpful-variable'."
  (interactive
   (list (helpful--read-symbol "Symbol: " #'helpful--bound-p)))
  (cond
   ((and (boundp symbol) (fboundp symbol))
    (if (y-or-n-p
         (format "%s is a both a variable and a callable, show variable?"
                 symbol))
        (helpful-variable symbol)
      (helpful-callable symbol)))
   ((fboundp symbol)
    (helpful-callable symbol))
   ((boundp symbol)
    (helpful-variable symbol))))

;;;###autoload
(defun helpful-variable (symbol)
  "Show help for variable named SYMBOL."
  (interactive
   (list (helpful--read-symbol "Variable: " #'helpful--variable-p)))
  (pop-to-buffer (helpful--buffer symbol nil))
  (helpful-update))

;;;###autoload
(defun helpful-at-point ()
  "Show help for the symbol at point."
  (interactive)
  (-if-let (symbol (symbol-at-point))
      (helpful-symbol symbol)
    (user-error "There is no symbol at point.")))

(define-derived-mode helpful-mode special-mode "Helpful"
  "Major mode for *Helpful* buffers."
  (add-hook 'xref-backend-functions #'elisp--xref-backend nil t))

(defun helpful-visit-reference ()
  "Go to the reference at point."
  (interactive)
  (let* ((path (get-text-property (point) 'helpful-path))
         (pos (get-text-property (point) 'helpful-pos))
         (pos-is-start (get-text-property (point) 'helpful-pos-is-start)))
    (when (and path pos)
      ;; If we're looking at a source excerpt, calculate the offset of
      ;; point, so we don't just go the start of the excerpt.
      (when pos-is-start
        (save-excursion
          (let ((offset 0))
            (while (and
                    (get-text-property (point) 'helpful-pos)
                    (not (eobp)))
              (backward-char 1)
              (setq offset (1+ offset)))
            ;; On the last iteration we moved outside the source
            ;; excerpt, so we overcounted by one character.
            (setq offset (1- offset))

            ;; Set POS so we go to exactly the place in the source
            ;; code where point was in the helpful excerpt.
            (setq pos (+ pos offset)))))

      (find-file path)
      (goto-char pos))))

(defun helpful--forward-button (direction)
  "Move point the next/previous button."
  (let ((step (if (< direction 0) -1 1)))
    ;; Step over the current button, if any.
    (while (and
            (not (if (< direction 0) (bobp) (eobp)))
            (get-text-property (point) 'button))
      (forward-char step))
    ;; Move forward until we hit a button.
    (while (and
            (not (if (< direction 0) (bobp) (eobp)))
            (not (get-text-property (point) 'button)))
      (forward-char step))
    ;; Ensure we're on the first char of the button.
    (while (and
            (not (if (< direction 0) (bobp) (eobp)))
            (get-text-property (point) 'button))
      (forward-char -1))
    (unless (bobp)
      (forward-char 1))))

(defun helpful-forward-button ()
  "Move point forward to the next button."
  (interactive)
  (helpful--forward-button 1))

(defun helpful-backward-button ()
  "Move point backward to the next button."
  (interactive)
  (helpful--forward-button -1))

(define-key helpful-mode-map (kbd "g") #'helpful-update)
(define-key helpful-mode-map (kbd "RET") #'helpful-visit-reference)

(define-key helpful-mode-map (kbd "TAB") #'helpful-forward-button)
(define-key helpful-mode-map (kbd "<backtab>") #'helpful-backward-button)

(provide 'helpful)
;;; helpful.el ends here
