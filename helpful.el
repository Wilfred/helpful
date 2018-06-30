;;; helpful.el --- a better *help* buffer            -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; URL: https://github.com/Wilfred/helpful
;; Keywords: help, lisp
;; Version: 0.12
;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (dash-functional "1.2.0") (s "1.11.0") (f "0.20.0") (elisp-refs "1.2") (shut-up "0.3"))

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
(require 'help-fns)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'f)
(require 'shut-up)
(require 'find-func)
(require 'nadvice)
(require 'info-look)
(require 'edebug)
(require 'trace)
(require 'imenu)

(defvar-local helpful--sym nil)
(defvar-local helpful--callable-p nil)
(defvar-local helpful--associated-buffer nil
  "We store a reference to the buffer we were called from, so we can
show the value of buffer-local variables.")
(defvar-local helpful--view-literal t
  "Whether to show a value as a literal, or a pretty interactive
view.")

(defgroup helpful nil
  "A rich help system with contextual information."
  :link '(url-link "https://github.com/Wilfred/helpful")
  :group 'help)

(defcustom helpful-max-buffers
  5
  "Helpful will kill the least recently used Helpful buffer
if there are more than this many.

To disable cleanup entirely, set this variable to nil. See also
`helpful-kill-buffers' for a one-off cleanup."
  :type '(choice (const nil) number)
  :group 'helpful)

(defcustom helpful-switch-buffer-function
  #'pop-to-buffer
  "Function called to display the *Helpful* buffer."
  :type 'function
  :group 'helpful)

;; TODO: explore whether more basic highlighting is fast enough to
;; handle larger functions. See `c-font-lock-init' and its use of
;; font-lock-keywords-1.
(defconst helpful-max-highlight 5000
  "Don't highlight code with more than this many characters.

This is currently only used for C code, as lisp highlighting
seems to be more efficient. This may change again in future.

See `this-command' as an example of a large piece of C code that
can make Helpful very slow.")

(defun helpful--kind-name (symbol callable-p)
  "Describe what kind of symbol this is."
  (cond
   ((not callable-p) "variable")
   ((commandp symbol) "command")
   ((macrop symbol) "macro")
   ((functionp symbol) "function")
   ((special-form-p symbol) "special form")))

(defun helpful--buffer (symbol callable-p)
  "Return a buffer to show help for SYMBOL in."
  (let* ((current-buffer (current-buffer))
         (buf-name
          (format "*helpful %s*"
                  (if (symbolp symbol)
                      (format "%s: %s"
                              (helpful--kind-name symbol callable-p)
                              symbol)
                    "lambda")))
         (buf (get-buffer buf-name)))
    (unless buf
      ;; If we need to create the buffer, ensure we don't exceed
      ;; `helpful-max-buffers' by killing the least recently used.
      (when (numberp helpful-max-buffers)
        (let* ((buffers (buffer-list))
               (helpful-bufs (--filter (with-current-buffer it
                                         (eq major-mode 'helpful-mode))
                                       buffers))
               ;; `buffer-list' seems to be ordered by most recently
               ;; visited first, so keep those.
               (excess-buffers (-drop (1- helpful-max-buffers) helpful-bufs)))
          ;; Kill buffers so we have one buffer less than the maximum
          ;; before we create a new one.
          (-each excess-buffers #'kill-buffer)))

      (setq buf (get-buffer-create buf-name)))

    ;; Initialise the buffer with the symbol and associated data.
    (with-current-buffer buf
      (helpful-mode)
      (setq helpful--sym symbol)
      (setq helpful--callable-p callable-p)
      (setq helpful--associated-buffer current-buffer))
    buf))

(defface helpful-heading
  '((t (:weight bold)))
  "Face used for headings in Helpful buffers.")

(defun helpful--heading (text)
  "Propertize TEXT as a heading."
  (format "%s\n" (propertize text 'face 'helpful-heading)))

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

If VALUE is self-referential, or just very big, the user may
press \\[keyboard-quit] to gracefully stop the printing."
  ;; Inspired by `ielm-eval-input'.
  (condition-case nil
      (s-trim-right (pp-to-string value))
    (quit
     (propertize "(User quit during pretty-printing.)"
                 'face 'font-lock-comment-face))))

(defun helpful--button (text type &rest properties)
  ;; `make-text-button' mutates our string to add properties. Copy
  ;; TEXT to prevent mutating our arguments, and to support 'pure'
  ;; strings, which are read-only.
  (setq text (substring-no-properties text))
  (apply #'make-text-button
         text nil
         :type type
         properties))

(defun helpful--canonical-symbol (sym callable-p)
  "If SYM is an alias, return the underlying symbol.
Return SYM otherwise."
  (let ((depth 0))
    (if (and (symbolp sym) callable-p)
        (while (and (symbolp (symbol-function sym))
                    (< depth 10))
          (setq sym (symbol-function sym))
          (setq depth (1+ depth)))
      (setq sym (indirect-variable sym))))
  sym)

(defun helpful--aliases (sym callable-p)
  "Return all the aliases for SYM."
  (let ((canonical (helpful--canonical-symbol sym callable-p))
        aliases)
    (mapatoms
     (lambda (s)
       (when (and
              ;; Skip variables that aren't bound, so we're faster.
              (if callable-p (fboundp s) (boundp s))

              ;; If this symbol is a new alias for our target sym,
              ;; add it.
              (eq canonical (helpful--canonical-symbol s callable-p))

              ;; Don't include SYM.
              (not (eq sym s)))
         (push s aliases))))
    (--sort
     (string< (symbol-name it) (symbol-name other))
     aliases)))

(defun helpful--format-alias (sym callable-p)
  (let ((obsolete-info (if callable-p
                           (get sym 'byte-obsolete-info)
                         (get sym 'byte-obsolete-variable)))
        (sym-button (helpful--button
                     (symbol-name sym)
                     'helpful-describe-exactly-button
                     'symbol sym
                     'callable-p callable-p)))
    (cond
     (obsolete-info
      (-if-let (version (-last-item obsolete-info))
          (format "%s (obsolete since %s)" sym-button version)
        (format "%s (obsolete)" sym-button)))
     (t
      sym-button))))

(defun helpful--indent-rigidly (s amount)
  "Indent string S by adding AMOUNT spaces to each line."
  (with-temp-buffer
    (insert s)
    (indent-rigidly (point-min) (point-max) amount)
    (buffer-string)))

(defun helpful--format-properties (symbol)
  "Return a string describing all the properties of SYMBOL."
  (let* ((syms-and-vals
          (-partition 2 (and (symbolp symbol) (symbol-plist symbol))))
         (syms-and-vals
          (-sort (-lambda ((sym1 _) (sym2 _))
                   (string-lessp (symbol-name sym1) (symbol-name sym2)))
                 syms-and-vals))
         (lines
          (--map
           (-let* (((sym val) it)
                   (pretty-val
                    (helpful--pretty-print val)))
             (format "%s\n%s%s"
                     (propertize (symbol-name sym)
                                 'face 'font-lock-constant-face)
                     (helpful--indent-rigidly pretty-val 2)
                     (cond
                      ;; Also offer to disassemble byte-code
                      ;; properties.
                      ((byte-code-function-p val)
                       (format "\n  %s"
                               (helpful--make-disassemble-button val)))
                      ((eq sym 'ert--test)
                       (format "\n  %s"
                               (helpful--make-run-test-button symbol)))
                      (t
                       ""))))
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
         (kind (helpful--kind-name sym callable-p)))
    (when (yes-or-no-p (format "Forget %s %s?" kind sym))
      (if callable-p
          (fmakunbound sym)
        (makunbound sym))
      (message "Forgot %s %s." kind sym)
      (kill-buffer (current-buffer)))))

(define-button-type 'helpful-c-source-directory
  'action #'helpful--c-source-directory
  'follow-link t
  'help-echo "Set directory to Emacs C source code")

(defun helpful--c-source-directory (_button)
  "Set `find-function-C-source-directory' so we can show the
source code to primitives."
  (let ((emacs-src-dir (read-directory-name "Path to Emacs source code: ")))
    ;; Let the user specify the source path with or without src/,
    ;; which is a subdirectory in the Emacs tree.
    (unless (equal (f-filename emacs-src-dir) "src")
      (setq emacs-src-dir (f-join emacs-src-dir "src")))
    (setq find-function-C-source-directory emacs-src-dir))
  (helpful-update))

(define-button-type 'helpful-disassemble-button
  'action #'helpful--disassemble
  'follow-link t
  'object nil
  'help-echo "Show disassembled bytecode")

(defun helpful--disassemble (button)
  "Disassemble the current symbol."
  ;; `disassemble' can handle both symbols (e.g. 'when) and raw
  ;; byte-code objects.
  (disassemble (button-get button 'object)))

(define-button-type 'helpful-run-test-button
  'action #'helpful--run-test
  'follow-link t
  'symbol nil
  'help-echo "Run ERT test")

(defun helpful--run-test (button)
  "Disassemble the current symbol."
  (ert (button-get button 'symbol)))

(define-button-type 'helpful-edebug-button
  'action #'helpful--edebug
  'follow-link t
  'symbol nil
  'help-echo "Toggle edebug (re-evaluates definition)")

(defun helpful--edebug-p (sym)
  "Does function SYM have its definition patched by edebug?"
  (let ((fn-def (indirect-function sym)))
    ;; Edebug replaces function source code with a sexp that has
    ;; `edebug-enter', `edebug-after' etc interleaved. This means the
    ;; function is interpreted, so `indirect-function' returns a list.
    (when (and (consp fn-def) (consp (cdr fn-def)))
      (-let [fn-end (-last-item fn-def)]
        (and (consp fn-end)
             (eq (car fn-end) 'edebug-enter))))))

(defun helpful--can-edebug-p (sym callable-p)
  "Can we use edebug with SYM?"
  (and
   ;; SYM must be a function.
   callable-p
   ;; The function cannot be a primitive, it must be defined in elisp.
   (not (helpful--primitive-p sym callable-p))
   ;; We need to be able to find its definition, or we can't step
   ;; through the source.
   (-let* (((buf pos opened) (helpful--definition sym t))
           (have-definition (and buf pos)))
     (when opened
       (kill-buffer buf))
     have-definition)))

(defun helpful--toggle-edebug (sym)
  "Enable edebug when function SYM is called,
or disable if already enabled."
  (-let ((should-edebug (not (helpful--edebug-p sym)))
         ((buf pos created) (helpful--definition sym t)))
    (if (and buf pos)
        (progn
          (with-current-buffer buf
            (save-excursion
              (save-restriction
                (widen)
                (goto-char pos)

                (let* ((edebug-all-forms should-edebug)
                       (edebug-all-defs should-edebug)
                       (form (edebug-read-top-level-form)))
                  ;; Based on `edebug-eval-defun'.
                  (eval (eval-sexp-add-defvars form) lexical-binding)))))
          ;; If we're enabling edebug, we need the source buffer to
          ;; exist. Otherwise, we can clean it up.
          (when (and created (not should-edebug))
            (kill-buffer buf)))

      (user-error "Could not find source for edebug"))))

(defun helpful--edebug (button)
  "Toggle edebug for the current symbol."
  (helpful--toggle-edebug (button-get button 'symbol))
  (helpful-update))

(define-button-type 'helpful-trace-button
  'action #'helpful--trace
  'follow-link t
  'symbol nil
  'help-echo "Toggle function tracing")

(defun helpful--trace (button)
  "Toggle tracing for the current symbol."
  (let ((sym (button-get button 'symbol)))
    (if (trace-is-traced sym)
        (untrace-function sym)
      (trace-function sym)))
  (helpful-update))

(define-button-type 'helpful-navigate-button
  'action #'helpful--navigate
  'path nil
  'position nil
  'follow-link t
  'help-echo "Navigate to definition")

(defun helpful--navigate (button)
  "Navigate to the path this BUTTON represents."
  (find-file (substring-no-properties (button-get button 'path)))
  ;; We use `get-text-property' to work around an Emacs 25 bug:
  ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=f7c4bad17d83297ee9a1b57552b1944020f23aea
  (-when-let (pos (get-text-property button 'position
                                     (marker-buffer button)))
    (goto-char pos)))

(defun helpful--navigate-button (text path &optional pos)
  "Return a button that opens PATH and puts point at POS."
  (helpful--button
   text
   'helpful-navigate-button
   'path path
   'position pos))

(define-button-type 'helpful-buffer-button
  'action #'helpful--switch-to-buffer
  'buffer nil
  'position nil
  'follow-link t
  'help-echo "Switch to this buffer")

(defun helpful--switch-to-buffer (button)
  "Navigate to the buffer this BUTTON represents."
  (let ((buf (button-get button 'buffer))
        (pos (button-get button 'position)))
    (switch-to-buffer buf)
    (goto-char pos)))

(defun helpful--buffer-button (buffer pos)
  "Return a button that switches to BUFFER and puts point at POS."
  (helpful--button
   (buffer-name buffer)
   'helpful-buffer-button
   'buffer buffer
   'position pos))

(define-button-type 'helpful-customize-button
  'action #'helpful--customize
  'symbol nil
  'follow-link t
  'help-echo "Open Customize for this symbol")

(defun helpful--customize (button)
  "Open Customize for this symbol."
  (customize-variable (button-get button 'symbol)))

(define-button-type 'helpful-toggle-button
  'action #'helpful--toggle
  'symbol nil
  'buffer nil
  'follow-link t
  'help-echo "Toggle this symbol between t and nil")

(defun helpful--toggle (button)
  "Toggle the symbol between nil and t."
  (let ((sym (button-get button 'symbol))
        (buf (button-get button 'buffer)))
    (save-current-buffer
      ;; If this is a buffer-local variable, ensure we're in the right
      ;; buffer.
      (when buf
        (set-buffer buf))
      (set sym (not (symbol-value sym))))
    (helpful-update)))

(define-button-type 'helpful-set-button
  'action #'helpful--set
  'symbol nil
  'buffer nil
  'follow-link t
  'help-echo "Set the value of this symbol")

(defun helpful--set (button)
  "Set the value of this symbol."
  (let* ((sym (button-get button 'symbol))
         (buf (button-get button 'buffer))
         (sym-value (helpful--sym-value sym buf))
         ;; Inspired by `counsel-read-setq-expression'.
         (expr
          (minibuffer-with-setup-hook
              (lambda ()
                (add-function :before-until (local 'eldoc-documentation-function)
                              #'elisp-eldoc-documentation-function)
                (run-hooks 'eval-expression-minibuffer-setup-hook)
                (goto-char (minibuffer-prompt-end))
                (forward-char (length (format "(setq %S " sym))))
            (read-from-minibuffer
             "Eval: "
             (format
              (if (or (symbolp sym-value) (consp sym-value))
                  "(setq %s '%S)"
                "(setq %s %S)")
              sym sym-value)
             read-expression-map t
             'read-expression-history))))
    (save-current-buffer
      ;; If this is a buffer-local variable, ensure we're in the right
      ;; buffer.
      (when buf
        (set-buffer buf))
      (eval-expression expr))
    (helpful-update)))

(define-button-type 'helpful-view-literal-button
  'action #'helpful--view-literal
  'help-echo "Toggle viewing as a literal")

(defun helpful--view-literal (_button)
  "Set the value of this symbol."
  (setq helpful--view-literal
        (not helpful--view-literal))
  (helpful-update))

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

(define-button-type 'helpful-manual-button
  'action #'helpful--manual
  'symbol nil
  'follow-link t
  'help-echo "View this symbol in the Emacs manual")

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

(define-button-type 'helpful-describe-exactly-button
  'action #'helpful--describe-exactly
  'symbol nil
  'callable-p nil
  'follow-link t
  'help-echo "Describe this symbol")

(defun helpful--describe-exactly (button)
  "Describe the symbol that this BUTTON represents.
This differs from `helpful--describe' because here we know
whether the symbol represents a variable or a callable."
  (let ((sym (button-get button 'symbol))
        (callable-p (button-get button 'callable-p)))
    (if callable-p
        (helpful-callable sym)
      (helpful-variable sym))))

(define-button-type 'helpful-info-button
  'action #'helpful--info
  'info-node nil
  'follow-link t
  'help-echo "View this Info node")

(defun helpful--info (button)
  "Describe the symbol that this BUTTON represents."
  (info (button-get button 'info-node)))

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

(defun helpful--propertize-keywords (docstring)
  "Propertize quoted keywords in docstrings."
  (replace-regexp-in-string
   ;; Replace all text of the form `foo'.
   (rx "`"
       (group ":"
              symbol-start
              (+? (or (syntax word) (syntax symbol)))
              symbol-end)
       "'")
   (lambda (it)
     (propertize (match-string 1 it)
                 'face 'font-lock-builtin-face))
   docstring
   t t))

(defun helpful--propertize-quoted (docstring)
  "Convert `foo' in docstrings to buttons (if bound) or else highlight."
  (replace-regexp-in-string
   ;; Replace all text of the form `foo'.
   (rx (? "\\=") "`" (+? (not (any "`" "'"))) "'")
   (lambda (it)
     (let* ((sym-name
             (s-chop-prefix "`" (s-chop-suffix "'" it)))
            (sym (intern sym-name)))
       (cond
        ;; If the quote is escaped, don't modify it.
        ((s-starts-with-p "\\=" it)
         it)
        ;; Only create a link if this is a symbol that is bound as a
        ;; variable or callable.
        ((or (boundp sym) (fboundp sym))
         (helpful--button
          sym-name
          'helpful-describe-button
          'symbol sym))
        ;; If this is already a button, don't modify it.
        ((get-text-property 0 'button sym-name)
         sym-name)
        ;; Highlight the quoted string.
        (t
         (propertize sym-name
                     'face 'font-lock-constant-face)))))
   docstring
   t t))

(defun helpful--propertize-info (docstring)
  "Convert info references in docstrings to buttons."
  (replace-regexp-in-string
   ;; Replace all text of the form `foo'.
   (rx "Info "
       (group
        (or "anchor" "node")
        (+ whitespace))
       "`"
       (group (+ (not (in "'"))))
       "'")
   (lambda (it)
     ;; info-node has the form "(cl)Loop Facility".
     (let ((space (match-string 1 it))
           (info-node (match-string 2 it)))
       (concat
        "Info "
        space
        (helpful--button
         info-node
         'helpful-info-button
         'info-node info-node))))
   docstring
   t t))

(defun helpful--char-table-keys (char-table)
  "Convert CHAR-TABLE to list of pairs (KEYCODES COMMAND)."
  ;; Kludge: use `describe-vector' to convert a char-table to a sparse
  ;; keymap.
  (let (result)
    (map-char-table
     (lambda (key value)
       (push (list (vector key) value) result))
     char-table)
    result))

(defun helpful--keymap-keys (keymap)
  "Return all the keys and commands in KEYMAP.
Flattens nested keymaps and follows remapped commands.

Returns a list of pairs (KEYCODES COMMAND), where KEYCODES is a
vector suitable for `key-description', and COMMAND is a smbol."
  (cond
   ;; Prefix keys.
   ((and
     (symbolp keymap)
     (fboundp keymap)
     ;; Prefix keys use a keymap in the function slot of a symbol.
     (keymapp (symbol-function keymap)))
    (helpful--keymap-keys (symbol-function keymap)))
   ;; Other symbols mean we've reached a leaf, so this is a command
   ;; we can call.
   ((symbolp keymap)
    `(([] ,keymap)))
   ((stringp (car keymap))
    (helpful--keymap-keys (cdr keymap)))
   ;; Otherwise, recurse on the keys at this level of the keymap.
   (t
    (let (result)
      (dolist (item (cdr keymap))
        (cond
         ((and (consp item)
               (eq (car item) 'menu-bar))
          ;; Skip menu bar items.
          nil)
         ;; Sparse keymaps are lists.
         ((consp item)
          (-let [(keycode . value) item]
            (-each (helpful--keymap-keys value)
              (-lambda ((keycodes command))
                (push (list (vconcat (vector keycode) keycodes) command)
                      result)))))
         ;; Dense keymaps are char-tables.
         ((char-table-p item)
          (map-char-table
           (lambda (keycode value)
             (-each (helpful--keymap-keys value)
               (-lambda ((keycodes command))
                 (push (list (vconcat (vector keycode) keycodes) command)
                       result))))
           item))))
      ;; For every command `new-func' mapped to a command `orig-func', show `new-func' with
      ;; the key sequence for `orig-func'.
      (setq result
            (-map-when
             (-lambda ((keycodes _))
               (and (> (length keycodes) 1)
                    (eq (elt keycodes 0) 'remap)))
             (-lambda ((keycodes command))
               (list
                (where-is-internal (elt keycodes 1) global-map t)
                command))
             result))
      ;; Preserve the original order of the keymap.
      (nreverse result)))))

(defun helpful--format-hook (hook-val)
  "Given a list value assigned to a hook, format it with links to functions."
  (let ((lines
         (--map
          (if (and (symbolp it) (fboundp it))
              (helpful--button
               (symbol-name it)
               'helpful-describe-exactly-button
               'symbol it
               'callable-p t)
            (helpful--syntax-highlight (helpful--pretty-print it)))
          hook-val)))
    (format "(%s)"
            (s-join "\n " lines))))

;; TODO: unlike `substitute-command-keys', this shows keybindings
;; which are currently shadowed (e.g. a global minor mode map).
(defun helpful--format-keymap (keymap)
  "Format KEYMAP."
  (let* ((keys-and-commands (helpful--keymap-keys keymap))
         ;; Convert keycodes [27 i] to "C-M-i".
         (keys (-map #'-first-item keys-and-commands))
         ;; Add padding so all our strings are the same length.
         (formatted-keys (-map #'key-description keys))
         (max-formatted-length (-max (cons 0 (-map #'length formatted-keys))))
         (aligned-keys (--map (s-pad-right (1+ max-formatted-length)
                                           " " it)
                              formatted-keys))
         ;; Format commands as buttons.
         (commands (-map (-lambda ((_ command)) command)
                         keys-and-commands))
         (formatted-commands
          (--map
           (helpful--button
            (symbol-name it)
            'helpful-describe-button
            'symbol it)
           commands))
         ;; Build lines for display.
         (lines
          (-map (-lambda ((key . command)) (format "%s %s" key command))
                (-zip-pair aligned-keys formatted-commands))))
    ;; The flattened keymap will have normal bindings first, and
    ;; inherited bindings last. Sort so that we group by prefix.
    (s-join "\n" (-sort #'string< lines))))

(defun helpful--format-command-keys (docstring)
  "Convert command key references and keymap references
in DOCSTRING to buttons.

Emacs uses \\= to escape \\[ references, so replace that
unescaping too."
  ;; Based on `substitute-command-keys', but converts command
  ;; references to buttons.
  (let ((keymap nil))
    (with-temp-buffer
      (insert docstring)
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at
           ;; Text of the form \=X
           (rx "\\="))
          ;; Remove the escaping, then step over the escaped char.
          ;; Step over the escaped character.
          (delete-region (point) (+ (point) 2))
          (forward-char 1))
         ((looking-at
           ;; Text of the form \\<foo-keymap>
           (rx "\\<" (group (+ (not (in ">")))) ">"))
          (let* ((symbol-with-parens (match-string 0))
                 (symbol-name (match-string 1)))
            ;; Remove the original string.
            (delete-region (point)
                           (+ (point) (length symbol-with-parens)))
            ;; Set the new keymap.
            (setq keymap (symbol-value (intern symbol-name)))))
         ((looking-at
           ;; Text of the form \\{foo-mode-map}
           (rx "\\{" (group (+ (not (in "}")))) "}"))
          (let* ((symbol-with-parens (match-string 0))
                 (symbol-name (match-string 1))
                 (keymap
                  ;; Gracefully handle variables not being defined.
                  (ignore-errors
                    (symbol-value (intern symbol-name)))))
            ;; Remove the original string.
            (delete-region (point)
                           (+ (point) (length symbol-with-parens)))
            (if keymap
                (insert (helpful--format-keymap keymap))
              (insert (format "Keymap %s is not currently defined."
                              symbol-name)))))
         ((looking-at
           ;; Text of the form \\[foo-command]
           (rx "\\[" (group (+ (not (in "]")))) "]"))
          (let* ((symbol-with-parens (match-string 0))
                 (symbol-name (match-string 1)))
            ;; Remove the original string.
            (delete-region (point)
                           (+ (point) (length symbol-with-parens)))
            ;; Add a button.
            (let* ((symbol (intern symbol-name))
                   (key (where-is-internal symbol keymap t))
                   (key-description
                    (if key
                        (key-description key)
                      (format "M-x %s" symbol-name))))
              (insert
               (helpful--button
                key-description
                'helpful-describe-exactly-button
                'symbol symbol
                'callable-p t)))))
         ;; Don't modify other characters.
         (t
          (forward-char 1))))
      (buffer-string))))

;; TODO: fix upstream Emacs bug that means `-map' is not highlighted
;; in the docstring for `--map'.
(defun helpful--format-docstring (docstring)
  "Replace cross-references with links in DOCSTRING."
  (-> docstring
      (helpful--split-first-line)
      (helpful--propertize-info)
      (helpful--propertize-links)
      (helpful--propertize-bare-links)
      (helpful--propertize-keywords)
      (helpful--propertize-quoted)
      ;; This needs to happen after we've replaced quoted chars, so we
      ;; don't confuse \\=` with `.
      (helpful--format-command-keys)
      (s-trim)))

(define-button-type 'helpful-link-button
  'action #'helpful--follow-link
  'follow-link t
  'help-echo "Follow this link")

(defun helpful--propertize-links (docstring)
  "Convert URL links in docstrings to buttons."
  (replace-regexp-in-string
   (rx "URL `" (group (*? any)) "'")
   (lambda (match)
     (let ((url (match-string 1 match)))
       (concat "URL "
               (helpful--button
                url
                'helpful-link-button
                'url url))))
   docstring))

(defun helpful--propertize-bare-links (docstring)
  "Convert URL links in docstrings to buttons."
  (replace-regexp-in-string
   (rx (group (or string-start space "<"))
       (group "http" (? "s") "://" (+? (not (any space))))
       (group (? (any "." ">" ")"))
              (or space string-end ">")))
   (lambda (match)
     (let ((space-before (match-string 1 match))
           (url (match-string 2 match))
           (after (match-string 3 match)))
       (concat
        space-before
        (helpful--button
         url
         'helpful-link-button
         'url url)
        after)))
   docstring))

(defun helpful--follow-link (button)
  "Follow the URL specified by BUTTON."
  (browse-url (button-get button 'url)))

(defconst helpful--highlighting-funcs
  '(ert--activate-font-lock-keywords
    highlight-quoted-mode
    rainbow-delimiters-mode)
  "Highlighting functions that are safe to run in a temporary buffer.
This is used in `helpful--syntax-highlight' to support extra
highlighting that the user may have configured in their mode
hooks.")

;; TODO: crashes on `backtrace-frame' on a recent checkout.

(defun helpful--syntax-highlight (source &optional mode)
  "Return a propertized version of SOURCE in MODE."
  (unless mode
    (setq mode #'emacs-lisp-mode))
  (if (or
       (< (length source) helpful-max-highlight)
       (eq mode 'emacs-lisp-mode))
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
        (buffer-string))
    ;; SOURCE was too long to highlight in a reasonable amount of
    ;; time.
    (concat
     (propertize
      "// Skipping highlighting due to "
      'face 'font-lock-comment-face)
     (helpful--button
      "helpful-max-highlight"
      'helpful-describe-exactly-button
      'symbol 'helpful-max-highlight
      'callable-p nil)
     (propertize
      ".\n"
      'face 'font-lock-comment-face)
     source)))

(defun helpful--source (sym callable-p)
  "Return the source code of SYM.
If the source code cannot be found, return the sexp used."
  (catch 'source
    (unless (symbolp sym)
      (throw 'source sym))

    (-let (((buf start-pos created) (helpful--definition sym callable-p))
           (source nil))
      (when (and buf start-pos)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (goto-char start-pos)
              (narrow-to-defun t)

              ;; If there was a preceding comment, START-POS will be
              ;; after that comment. Move the position to include that comment.
              (setq start-pos (point-min))

              (setq source (buffer-substring-no-properties (point-min) (point-max))))))
        (setq source (s-trim-right source))
        (when (and source (buffer-file-name buf))
          (setq source (propertize source
                                   'helpful-path (buffer-file-name buf)
                                   'helpful-pos start-pos
                                   'helpful-pos-is-start t)))
        (when (and buf created)
          (kill-buffer buf))
        (throw 'source source)))

    (when callable-p
      ;; Could not find source -- probably defined interactively, or via
      ;; a macro, or file has changed.
      ;; TODO: verify that the source hasn't changed before showing.
      ;; TODO: offer to download C sources for current version.
      (throw 'source (indirect-function sym)))))

(defun helpful--in-manual-p (sym)
  "Return non-nil if SYM is in an Info manual."
  (let ((completions
         (shut-up
           (info-lookup->completions 'symbol 'emacs-lisp-mode))))
    (-when-let (buf (get-buffer " temp-info-look"))
      (kill-buffer buf))
    (or (assoc sym completions)
        (assoc-string sym completions))))

(defun helpful--find-library-name (path)
  "A wrapper around `find-library-name' that returns nil if PATH
has no library with that name.

This can happen when users have installed Emacs without its
source code: they have e.g. org.elc but no org.el."
  (condition-case _err
      (find-library-name path)
    (error nil)))

(defun helpful--definition (sym callable-p)
  "Return a list (BUF POS OPENED) where SYM is defined.

BUF is the buffer containing the definition. If the user wasn't
already visiting this buffer, OPENED is t and callers should kill
the buffer when done.

POS is the position of the start of the definition within the
buffer."
  (-let ((initial-buffers (buffer-list))
         (primitive-p (helpful--primitive-p sym callable-p))
         (path nil)
         (buf nil)
         (pos nil)
         (opened nil)
         ;; If we end up opening a buffer, don't bother with file
         ;; variables. It prompts the user, and we discard the buffer
         ;; afterwards anyway.
         (enable-local-variables nil))
    (when (and (symbolp sym) callable-p)
      (-let [(_ . src-path) (find-function-library sym)]
        (setq path src-path)))
    (when (and primitive-p path find-function-C-source-directory)
      ;; Convert "src/foo.c" to "".
      (setq path (f-expand path
                           (f-parent find-function-C-source-directory))))

    (cond
     ((and (not (symbolp sym)) (functionp sym))
      (list nil nil nil))
     ((and callable-p path)
      ;; Convert foo.elc to foo.el.
      (-when-let (src-path (helpful--find-library-name path))
        ;; Open `path' ourselves, so we can widen before searching.
        ;;
        ;; Opening large.c files can be slow (e.g. when looking at
        ;; `defalias'), especially if the user has configured mode hooks.
        ;;
        ;; Bind `auto-mode-alist' to nil, so we open the buffer in
        ;; `fundamental-mode' if it isn't already open.
        (let ((auto-mode-alist nil))
          (setq buf (find-file-noselect src-path)))

        (unless (-contains-p initial-buffers buf)
          (setq opened t))

        ;; If it's a freshly opened buffer, we need to switch to the
        ;; correct mode so we can search correctly. Enable the mode, but
        ;; don't bother with mode hooks, because we just need the syntax
        ;; table for searching.
        (when opened
          (with-current-buffer buf
            (delay-mode-hooks (normal-mode t))))

        ;; Based on `find-function-noselect'.
        (with-current-buffer buf
          ;; `find-function-search-for-symbol' moves point. Prevent
          ;; that.
          (save-excursion
            ;; Narrowing has been fixed upstream:
            ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=abd18254aec76b26e86ae27e91d2c916ec20cc46
            (save-restriction
              (widen)
              (setq pos
                    (if primitive-p
                        (cdr (find-function-C-source sym path nil))
                      (cdr (find-function-search-for-symbol sym nil path)))))))))
     (callable-p
      ;; Functions defined interactively may have an edebug property
      ;; that contains the location of the definition.
      (-when-let (edebug-info (get sym 'edebug))
        (-let [marker (if (consp edebug-info)
                          (car edebug-info)
                        edebug-info)]
          (setq buf (marker-buffer marker))
          (setq pos (marker-position marker)))))
     ((not callable-p)
      (condition-case _err
          (-let [(sym-buf . sym-pos) (find-definition-noselect sym 'defvar)]
            (setq buf sym-buf)
            (unless (-contains-p initial-buffers buf)
              (setq opened t))
            (setq pos sym-pos))
        (search-failed nil)
        ;; If your current Emacs instance doesn't match the source
        ;; code configured in find-function-C-source-directory, we can
        ;; get an error about not finding source. Try
        ;; `default-tab-width' against Emacs trunk.
        (error nil))))
    (list buf pos opened)))

(defun helpful--source-path (sym callable-p)
  "Return the path where SYM is defined."
  (-let* (((buf _ opened) (helpful--definition sym callable-p))
          (path nil))
    (when buf
      (setq path (buffer-file-name buf))
      (when opened
        ;; If we've just created this buffer, close it.
        (kill-buffer buf)))
    path))

(defun helpful--source-pos (sym callable-p)
  "Return the file position where SYM is defined."
  (-let (((buf pos opened) (helpful--definition sym callable-p)))
    ;; If we've just created this buffer, close it.
    (when opened
      ;; If we've just created this buffer, close it.
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
       (when (and (boundp sym) (keymapp (symbol-value sym)))
         (push sym keymaps))))
    keymaps))

(defun helpful--key-sequences (command-sym keymap)
  "Return all the key sequences of COMMAND-SYM in KEYMAP."
  (let* ((keycodes
          ;; Look up this command in the keymap, its parent and the
          ;; global map. We need to include the global map to find
          ;; remapped commands.
          (where-is-internal command-sym keymap nil t))
         ;; Look up this command in the parent keymap.
         (parent-keymap (keymap-parent keymap))
         (parent-keycodes
          (when parent-keymap
            (where-is-internal
             command-sym (list parent-keymap) nil t)))
         ;; Look up this command in the global map.
         (global-keycodes
          (unless (eq keymap global-map)
            (where-is-internal
             command-sym (list global-map) nil t))))
    (->> keycodes
         ;; Ignore keybindings from the parent or global map.
         (--remove (-contains-p parent-keycodes it))
         (--remove (-contains-p global-keycodes it))
         ;; Convert raw keycode vectors into human-readable strings.
         (-map #'key-description))))

(defun helpful--keymaps-containing (command-sym)
  "Return a list of pairs listing keymap names that contain COMMAND-SYM,
along with the keybindings in each keymap.

Keymap names are typically variable names, but may also be
descriptions of values in `minor-mode-map-alist'.

We ignore keybindings that are menu items, and ignore keybindings
from parent keymaps.

`widget-global-map' is also ignored as it generally contains the
same bindings as `global-map'."
  (let* ((keymap-syms (helpful--all-keymap-syms))
         (keymap-sym-vals (-map #'symbol-value keymap-syms))
         matching-keymaps)
    ;; Look for this command in all keymaps bound to variables.
    (-map
     (-lambda ((keymap-sym . keymap))
       (let ((key-sequences (helpful--key-sequences command-sym keymap)))
         (when (and key-sequences (not (eq keymap-sym 'widget-global-map)))
           (push (cons (symbol-name keymap-sym) key-sequences)
                 matching-keymaps))))
     (-zip keymap-syms keymap-sym-vals))

    ;; Look for this command in keymaps used by minor modes that
    ;; aren't bound to variables.
    (-map
     (-lambda ((minor-mode . keymap))
       ;; Only consider this keymap if we didn't find it bound to a variable.
       (when (and (keymapp keymap)
                  (not (memq keymap keymap-sym-vals)))
         (let ((key-sequences (helpful--key-sequences command-sym keymap)))
           (when key-sequences
             (push (cons (format "minor-mode-map-alist (%s)" minor-mode)
                         key-sequences)
                   matching-keymaps)))))
     ;; TODO: examine `minor-mode-overriding-map-alist' too.
     minor-mode-map-alist)

    matching-keymaps))

(defun helpful--format-keys (command-sym)
  "Describe all the keys that call COMMAND-SYM."
  (let (mode-lines
        global-lines)
    (--each (helpful--keymaps-containing command-sym)
      (-let [(map . keys) it]
        (dolist (key keys)
          (push
           (format "%s %s"
                   (propertize map 'face 'font-lock-variable-name-face)
                   key)
           (if (eq map 'global-map) global-lines mode-lines)))))
    (setq global-lines (-sort #'string< global-lines))
    (setq mode-lines (-sort #'string< mode-lines))
    (-let [lines (-concat global-lines mode-lines)]
      (if lines
          (s-join "\n" lines)
        "This command is not in any keymaps."))))

(defun helpful--outer-sexp (buf pos)
  "Find position POS in BUF, and return the name of the outer sexp,
along with its position.

Moves point in BUF."
  (with-current-buffer buf
    (goto-char pos)
    (let* ((ppss (syntax-ppss)))
      (unless (zerop (syntax-ppss-depth ppss))
        (beginning-of-defun)))
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
  (and (symbolp sym)
       (advice--p (advice--symbol-function sym))))

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

(defun helpful--sym-value (sym buf)
  "Return the value of SYM in BUF."
  (with-current-buffer buf
    (symbol-value sym)))

(defun helpful--insert-section-break ()
  "Insert section break into helpful buffer."
  (insert "\n\n"))

(defun helpful--calculate-references (sym callable-p source-path)
  "Calculate references for SYM in SOURCE-PATH."
  (when source-path
    (let* ((primitive-p (helpful--primitive-p sym callable-p))
           (buf (elisp-refs--contents-buffer source-path))
           (positions
            (if primitive-p
                nil
              (helpful--reference-positions
               helpful--sym helpful--callable-p buf)))
           (return-value (--map (helpful--outer-sexp buf it) positions)))
      (kill-buffer buf)
      return-value)))

(defun helpful--make-manual-button (sym)
  "Make manual button for SYM."
  (helpful--button
   "View in manual"
   'helpful-manual-button
   'symbol sym))

(defun helpful--make-toggle-button (sym buffer)
  "Make toggle button for SYM in BUFFER."
  (helpful--button
   "Toggle"
   'helpful-toggle-button
   'symbol sym
   'buffer buffer))

(defun helpful--make-set-button (sym buffer)
  "Make set button for SYM in BUFFER."
  (helpful--button
   "Set"
   'helpful-set-button
   'symbol sym
   'buffer buffer))

(defun helpful--make-toggle-literal-button ()
  "Make set button for SYM in BUFFER."
  (helpful--button
   (if helpful--view-literal
       "Pretty view"
     "View as literal")
   'helpful-view-literal-button))

(defun helpful--make-customize-button (sym)
  "Make customize button for SYM."
  (helpful--button
   "Customize"
   'helpful-customize-button
   'symbol sym))

(defun helpful--make-references-button (sym callable-p)
  "Make references button for SYM."
  (helpful--button
   "Find all references"
   'helpful-all-references-button
   'symbol sym
   'callable-p callable-p))

(defun helpful--make-edebug-button (sym)
  "Make edebug button for SYM."
  (helpful--button
   (format "%s edebug"
           (if (helpful--edebug-p sym)
               "Disable" "Enable"))
   'helpful-edebug-button
   'symbol sym))

(defun helpful--make-tracing-button (sym)
  "Make tracing button for SYM."
  (helpful--button
   (format "%s tracing"
           (if (trace-is-traced sym)
               "Disable" "Enable"))
   'helpful-trace-button
   'symbol sym))

(defun helpful--make-disassemble-button (obj)
  "Make disassemble button for OBJ.
OBJ may be a symbol or a compiled function object."
  (helpful--button
   "Disassemble"
   'helpful-disassemble-button
   'object obj))

(defun helpful--make-run-test-button (sym)
  "Make an ERT test button for SYM."
  (helpful--button
   "Run test"
   'helpful-run-test-button
   'symbol sym))

(defun helpful--make-forget-button (sym callable-p)
  "Make forget button for SYM."
  (helpful--button
   "Forget"
   'helpful-forget-button
   'symbol sym
   'callable-p callable-p))

(defun helpful--summary (sym callable-p)
  "Return a one sentence summary for SYM."
  (-let* ((primitive-p (helpful--primitive-p sym callable-p))
          (canonical-sym (helpful--canonical-symbol sym callable-p))
          (alias-p (not (eq canonical-sym sym)))
          ((buf pos opened)
           (if (or (not primitive-p) find-function-C-source-directory)
               (helpful--definition sym callable-p)
             '(nil nil nil)))
          (alias-button
           (if callable-p
               ;; Show a link to 'defalias' in the manual.
               (helpful--button
                "function alias"
                'helpful-manual-button
                'symbol 'defalias)
             ;; Show a link to the variable aliases section in the
             ;; manual.
             (helpful--button
              "alias"
              'helpful-info-button
              'info-node "(elisp)Variable Aliases")))
          (interactive-button
           (helpful--button
            "interactive"
            'helpful-info-button
            'info-node "(elisp)Using Interactive"))
          (autoload-button
           (helpful--button
            "autoloaded"
            'helpful-info-button
            'info-node "(elisp)Autoload"))
          ;; TODO: this only reports if a function is autoloaded
          ;; because we autoloaded it. This ignores newly defined
          ;; functions that are autoloaded. Built-in help has this
          ;; limitation too, but if we can find the source, we should
          ;; instead see if there's an autoload cookie.
          (autoloaded-p
           (if (and callable-p buf (buffer-file-name buf))
               (help-fns--autoloaded-p sym (buffer-file-name buf))))
          (description
           (cond
            (alias-p
             (format "%s %s"
                     (if callable-p "a" "an")
                     alias-button))
            ((and callable-p (commandp sym) autoloaded-p)
             (format "an %s, %s" interactive-button autoload-button))
            ((and callable-p (commandp sym))
             (format "an %s" interactive-button))
            ((and callable-p autoloaded-p)
             (format "an %s" autoload-button))
            (t
             "a")))
          (kind
           (cond
            (alias-p
             (format "for %s,"
                     (helpful--button
                      (symbol-name canonical-sym)
                      'helpful-describe-exactly-button
                      'symbol canonical-sym
                      'callable-p callable-p)))
            ((not callable-p) "variable")
            ((macrop sym) "macro")
            (t "function")))
          (defined
            (cond
             (buf
              (let ((path (buffer-file-name buf)))
                (if path
                    (format
                     "defined in %s"
                     (helpful--navigate-button
                      (file-name-nondirectory path) path pos))
                  (format "defined in buffer %s"
                          (helpful--buffer-button buf pos)))))
             (primitive-p
              "defined in C source code")
             (t
              "without a source file"))))
    (when opened
      (kill-buffer buf))

    (s-word-wrap
     70
     (format "%s is %s %s %s."
             (if (symbolp sym) sym "This lambda")
             description kind defined))))

(defun helpful-update ()
  "Update the current *Helpful* buffer to the latest
state of the current symbol."
  (interactive)
  (cl-assert (not (null helpful--sym)))
  (unless (buffer-live-p helpful--associated-buffer)
    (setq helpful--associated-buffer nil))
  (let* ((inhibit-read-only t)
         (start-line (line-number-at-pos))
         (start-column (current-column))
         (primitive-p (helpful--primitive-p helpful--sym helpful--callable-p))
         (sym-type (cond
                    ((not helpful--callable-p) "Variable")
                    ((macrop helpful--sym) "Macro")
                    (t "Function")))
         (look-for-src (or (not primitive-p)
                           find-function-C-source-directory))
         (source (when look-for-src
                   (helpful--source helpful--sym helpful--callable-p)))
         (source-path (when (and look-for-src (symbolp helpful--sym))
                        (helpful--source-path helpful--sym helpful--callable-p)))
         (references (helpful--calculate-references
                      helpful--sym helpful--callable-p
                      source-path)))

    (erase-buffer)

    (insert (helpful--summary helpful--sym helpful--callable-p))

    (when helpful--callable-p
      (helpful--insert-section-break)
      (insert
       (helpful--heading (format "%s Signature" sym-type))
       (helpful--syntax-highlight (helpful--signature helpful--sym))))

    (-when-let (docstring (helpful--docstring helpful--sym helpful--callable-p))
      (helpful--insert-section-break)
      (insert
       (helpful--heading (format "%s Documentation" sym-type))
       (helpful--format-docstring docstring))
      (when (helpful--in-manual-p helpful--sym)
        (insert "\n\n")
        (insert (helpful--make-manual-button helpful--sym))))

    (when (not helpful--callable-p)
      (helpful--insert-section-break)
      (let* ((sym helpful--sym)
             (buf (or helpful--associated-buffer (current-buffer)))
             (val (helpful--sym-value sym buf))
             (multiple-views-p
              (or (stringp val)
                  (keymapp val)
                  (and (s-ends-with-p "-hook" (symbol-name sym))
                       (consp val)))))
        (insert
         (helpful--heading "Value")
         (cond
          (helpful--view-literal
           (helpful--pretty-print val))
          ;; Allow strings to be viewed with properties rendered in
          ;; Emacs, rather than as a literal.
          ((stringp val)
           val)
          ;; Allow keymaps to be viewed with keybindings shown and
          ;; links to the commands bound.
          ((keymapp val)
           (helpful--format-keymap val))
          ((and (s-ends-with-p "-hook" (symbol-name sym))
                (consp val))
           (helpful--format-hook val))
          (t
           (error "don't know how to format value of type %s"
                  (type-of val))))
         "\n\n")
        (when multiple-views-p
          (insert (helpful--make-toggle-literal-button) " "))
        (when (memq (helpful--sym-value helpful--sym buf) '(nil t))
          (insert (helpful--make-toggle-button helpful--sym buf) " "))
        (insert (helpful--make-set-button helpful--sym buf))
        (when (custom-variable-p helpful--sym)
          (insert " " (helpful--make-customize-button helpful--sym)))))

    ;; Show keybindings.
    ;; TODO: allow users to conveniently add and remove keybindings.
    (when (commandp helpful--sym)
      (helpful--insert-section-break)
      (insert
       (helpful--heading "Key Bindings")
       (helpful--format-keys helpful--sym)))

    (helpful--insert-section-break)

    (insert
     (helpful--heading "References")
     (let ((src-button
            (when source-path
              (helpful--navigate-button
               (file-name-nondirectory source-path)
               source-path
               (or (helpful--source-pos helpful--sym helpful--callable-p)
                   0)))))
       (cond
        ((and source-path references)
         (format "References in %s:\n%s"
                 src-button
                 (helpful--format-position-heads references source-path)))
        ((and source-path primitive-p)
         (format
          "Finding references in a .%s file is not supported."
          (f-ext source-path)))
        (source-path
         (format "%s is unused in %s."
                 helpful--sym
                 src-button))
        ((and primitive-p (null find-function-C-source-directory))
         "C code is not yet loaded.")
        (t
         "Could not find source file.")))
     "\n\n"
     (helpful--make-references-button helpful--sym helpful--callable-p))

    (when (helpful--advised-p helpful--sym)
      (helpful--insert-section-break)
      (insert
       (helpful--heading "Advice")
       (format "This %s is advised." (downcase sym-type))))

    (let ((can-edebug
           (helpful--can-edebug-p helpful--sym helpful--callable-p))
          (can-trace
           (and (symbolp helpful--sym)
                helpful--callable-p
                ;; Tracing uses advice, and you can't apply advice to
                ;; primitive functions that are replaced with special
                ;; opcodes. For example, `narrow-to-region'.
                (not (plist-get (symbol-plist helpful--sym) 'byte-opcode))))
          (can-disassemble
           (and helpful--callable-p (not primitive-p)))
          (can-forget
           (and (not (special-form-p helpful--sym))
                (not primitive-p))))
      (when (or can-edebug can-trace can-disassemble can-forget)
        (helpful--insert-section-break)
        (insert (helpful--heading "Debugging")))
      (when can-edebug
        (insert
         (helpful--make-edebug-button helpful--sym)))
      (when can-trace
        (when can-edebug
          (insert " "))
        (insert
         (helpful--make-tracing-button helpful--sym)))

      (when (and
             (or can-edebug can-trace)
             (or can-disassemble can-forget))
        (insert "\n"))

      (when can-disassemble
        (insert (helpful--make-disassemble-button helpful--sym)))

      (when can-forget
        (when can-disassemble
          (insert " "))
        (insert (helpful--make-forget-button helpful--sym helpful--callable-p))))

    (let ((aliases (helpful--aliases helpful--sym helpful--callable-p)))
      (when aliases
        (helpful--insert-section-break)
        (insert
         (helpful--heading "Aliases")
         (s-join "\n" (--map (helpful--format-alias it helpful--callable-p)
                             aliases)))))

    (helpful--insert-section-break)

    (insert
     (helpful--heading "Source Code")
     (cond
      (source-path
       (concat
        (propertize (format "%s Defined in " (if primitive-p "//" ";;"))
                    'face 'font-lock-comment-face)
        (helpful--navigate-button
         (f-abbrev source-path)
         source-path
         (helpful--source-pos helpful--sym helpful--callable-p))
        "\n"))
      (primitive-p
       (concat
        (propertize
         "C code is not yet loaded."
         'face 'font-lock-comment-face)
        "\n\n"
        (helpful--button
         "Set C source directory"
         'helpful-c-source-directory)))
      (t
       "")))
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
          (concat
           ";; Source file is unknown, showing raw function object.\n"
           (helpful--pretty-print source)))))))

    (helpful--insert-section-break)

    (-when-let (formatted-props (helpful--format-properties helpful--sym))
      (insert
       (helpful--heading "Symbol Properties")
       formatted-props))

    (goto-char (point-min))
    (forward-line (1- start-line))
    (forward-char start-column)))

;; TODO: this isn't sufficient for `edebug-eval-defun'.
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
        source-sig
        (advertised-args
         (when (symbolp sym)
           (gethash (symbol-function sym) advertised-signature-table))))
    ;; Get the usage from the function definition.
    (let* ((function-args
            (if (symbolp sym)
                (help-function-arglist sym)
              (cadr sym)))
           (formatted-args
            (cond
             (advertised-args
              (-map #'helpful--format-argument advertised-args))
             ((listp function-args)
              (-map #'helpful--format-argument function-args))
             (t
              (list function-args)))))
      (setq source-sig
            (cond
             ;; If it's a function object, just show the arguments.
             ((not (symbolp sym))
              (format "(%s)"
                      (s-join " " formatted-args)))
             ;; If it has multiple arguments, join them with spaces.
             (formatted-args
              (format "(%s %s)" sym
                      (s-join " " formatted-args)))
             ;; Otherwise, this function takes no arguments when called.
             (t
              (format "(%s)" sym)))))

    ;; If the docstring ends with (fn FOO BAR), extract that.
    (-when-let (docstring (documentation sym))
      (-when-let (docstring-with-usage (help-split-fundoc docstring sym))
        (setq docstring-sig (car docstring-with-usage))))

    (cond
     ;; Advertised signature always wins.
     (advertised-args
      source-sig)
     ;; If that's not set, use the usage specification in the
     ;; docstring, if present.
     (docstring-sig)
     (t
      ;; Otherwise, just use the signature from the source code.
      source-sig))))

(defun helpful--docstring (sym callable-p)
  "Get the docstring for SYM.
Note that this returns the raw docstring, including \\=\\=
escapes that are used by `substitute-command-keys'."
  (let ((text-quoting-style 'grave)
        docstring)
    (if callable-p
        (progn
          (setq docstring (documentation sym t))
          (-when-let (docstring-with-usage (help-split-fundoc docstring sym))
            (setq docstring (cdr docstring-with-usage))
            (when docstring
              ;; Advice mutates the docstring, see
              ;; `advice--make-docstring'. Undo that.
              ;; TODO: Only do this if the function is advised.
              (setq docstring (helpful--skip-advice docstring)))))
      (setq docstring
            (documentation-property sym 'variable-documentation t)))
    docstring))

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
  (funcall helpful-switch-buffer-function (helpful--buffer symbol t))
  (helpful-update))

;;;###autoload
(defun helpful-command (symbol)
  "Show help for interactive function named SYMBOL."
  (interactive
   (list (helpful--read-symbol "Command: " #'commandp)))
  (funcall helpful-switch-buffer-function (helpful--buffer symbol t))
  (helpful-update))

;;;###autoload
(defun helpful-key (key-sequence)
  "Show help for interactive command bound to KEY-SEQUENCE."
  (interactive
   (list (read-key-sequence "Press key: ")))
  (let ((sym (key-binding key-sequence)))
    (cond
     ((null sym)
      (user-error "No command is bound to %s"
                  (key-description key-sequence)))
     ((commandp sym)
      (funcall helpful-switch-buffer-function (helpful--buffer sym t))
      (helpful-update))
     (t
      (user-error "%s is bound to %s which is not a command"
                  (key-description key-sequence)
                  sym)))))

;;;###autoload
(defun helpful-macro (symbol)
  "Show help for macro named SYMBOL."
  (interactive
   (list (helpful--read-symbol "Macro: " #'macrop)))
  (funcall helpful-switch-buffer-function (helpful--buffer symbol t))
  (helpful-update))

;;;###autoload
(defun helpful-callable (symbol)
  "Show help for function, macro or special form named SYMBOL.

See also `helpful-macro' and `helpful-function'."
  (interactive
   (list (helpful--read-symbol "Callable: " #'fboundp)))
  (funcall helpful-switch-buffer-function (helpful--buffer symbol t))
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
    (helpful-variable symbol))
   (t
    (user-error "Not bound: %S" symbol))))

;;;###autoload
(defun helpful-variable (symbol)
  "Show help for variable named SYMBOL."
  (interactive
   (list (helpful--read-symbol "Variable: " #'helpful--variable-p)))
  (funcall helpful-switch-buffer-function (helpful--buffer symbol nil))
  (helpful-update))

;;;###autoload
(defun helpful-at-point ()
  "Show help for the symbol at point."
  (interactive)
  (-if-let (symbol (symbol-at-point))
      (helpful-symbol symbol)
    (user-error "There is no symbol at point.")))

(defun helpful--imenu-index ()
  "Return a list of headings in the current buffer, suitable for
imenu."
  (let (headings)
    (goto-char (point-min))
    (while (not (eobp))
      (when (eq (get-text-property (point) 'face)
                'helpful-heading)
        (push
         (cons
          (buffer-substring-no-properties
           (line-beginning-position) (line-end-position))
          (line-beginning-position))
         headings))
      (forward-line))
    (nreverse headings)))

(define-derived-mode helpful-mode special-mode "Helpful"
  "Major mode for *Helpful* buffers."
  (add-hook 'xref-backend-functions #'elisp--xref-backend nil t)

  (setq imenu-create-index-function #'helpful--imenu-index)
  ;; Prevent imenu converting "Source Code" to "Source.Code".
  (setq-local imenu-space-replacement " "))

(defun helpful--flash-region (start end)
  "Temporarily highlight region from START to END."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 1.0 nil 'delete-overlay overlay)))

(defun helpful-visit-reference ()
  "Go to the reference at point."
  (interactive)
  (let* ((sym helpful--sym)
         (path (get-text-property (point) 'helpful-path))
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
      (when (or (< pos (point-min))
                (> pos (point-max)))
        (widen))
      (goto-char pos)
      (recenter 0)
      (save-excursion
        (let ((defun-end (scan-sexps (point) 1)))
          (while (re-search-forward
                  (rx-to-string `(seq symbol-start ,(symbol-name sym) symbol-end))
                  defun-end t)
            (helpful--flash-region (match-beginning 0) (match-end 0))))))))

(defun helpful-kill-buffers ()
  "Kill all `helpful-mode' buffers.

See also `helpful-max-buffers'."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'major-mode buffer) 'helpful-mode)
      (kill-buffer buffer))))

(define-key helpful-mode-map (kbd "g") #'helpful-update)
(define-key helpful-mode-map (kbd "RET") #'helpful-visit-reference)

(define-key helpful-mode-map (kbd "TAB") #'forward-button)
(define-key helpful-mode-map (kbd "<backtab>") #'backward-button)

(provide 'helpful)
;;; helpful.el ends here
