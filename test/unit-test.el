(require 'ert)
(require 'edebug)
(require 'helpful)

(defun test-foo ()
  "Docstring here."
  nil)

(defun test-foo-advised ()
  "Docstring here too."
  nil)

(autoload 'some-unused-function "somelib.el")

(defadvice test-foo-advised (before test-advice1 activate)
  "Placeholder advice 1."
  nil)

(defadvice test-foo-advised (after test-advice2 activate)
  "Placeholder advice 2."
  nil)

(ert-deftest helpful--docstring ()
  "Basic docstring fetching."
  (should
   (equal
    (helpful--docstring #'test-foo t)
    "Docstring here.")))

(ert-deftest helpful--docstring-advice ()
  "Get the docstring on advised functions."
  (should
   (equal
    (helpful--docstring #'test-foo-advised t)
    "Docstring here too.")))

(defun test-foo-no-docstring ()
  nil)

(ert-deftest helpful--no-docstring ()
  "We should not crash on a function without a docstring."
  (should (null (helpful--docstring #'test-foo-no-docstring t))))

(ert-deftest helpful--interactively-defined-fn ()
  "We should not crash on a function without source code."
  (eval '(defun test-foo-defined-interactively () 42))
  (with-temp-buffer
    (helpful-function #'test-foo-defined-interactively)
    (should (equal (buffer-name) "*helpful function: test-foo-defined-interactively*"))))

(ert-deftest helpful--edebug-fn ()
  "We should not crash on a function with edebug enabled."
  (let ((edebug-all-forms t)
        (edebug-all-defs t))
    (with-temp-buffer
      (insert "(defun test-foo-edebug () 44)")
      (goto-char (point-min))
      (shut-up
        (eval (eval-sexp-add-defvars (edebug-read-top-level-form)) t))))
  (helpful-function #'test-foo-edebug))

(defun test-foo-usage-docstring ()
  "\n\n(fn &rest ARGS)"
  nil)

(ert-deftest helpful--usage-docstring ()
  "If a function docstring only has usage, do not return it."
  (should (null (helpful--docstring #'test-foo-usage-docstring t))))

(defun test-foo-no-properties ()
  nil)

(ert-deftest helpful--primitive-p ()
  ;; Defined in C.
  (should (helpful--primitive-p 'message t))
  ;; Defined in C, but an alias.
  (should (helpful--primitive-p 'not t))
  ;; Defined in elisp.
  (should (not (helpful--primitive-p 'when t))))

(ert-deftest helpful-callable ()
  ;; We should not crash when looking at macros.
  (helpful-callable 'when)
  ;; Special forms should work too.
  (helpful-callable 'if)
  ;; Smoke test for special forms when we have the Emacs C source
  ;; loaded.
  (let* ((emacs-src-path (f-join default-directory "emacs-25.3" "src")))
    (if (f-exists-p emacs-src-path)
        (let ((find-function-C-source-directory emacs-src-path))
          (helpful-callable 'if))
      (message "No Emacs source code found at %S, skipping test. Run ./download_emacs_src.sh."
               emacs-src-path))))

(ert-deftest helpful--no-symbol-properties ()
  "Helpful should handle functions without any symbol properties."
  ;; Interactively evaluating this file will set edebug properties on
  ;; test-foo, so remove all properties.
  (setplist #'test-foo-no-properties nil)

  ;; This shouldn't throw any errors.
  (helpful-function #'test-foo-no-properties))

(ert-deftest helpful--split-first-line ()
  ;; Don't modify a single line string.
  (should
   (equal (helpful--split-first-line "foo") "foo"))
  ;; Don't modify a two-line string if we don't end with .
  (should
   (equal (helpful--split-first-line "foo\nbar") "foo\nbar"))
  ;; If the second line is already empty, do nothing.
  (should
   (equal (helpful--split-first-line "foo.\n\nbar") "foo.\n\nbar"))
  ;; But if we have a single sentence and no empy line, insert one.
  (should
   (equal (helpful--split-first-line "foo.\nbar") "foo.\n\nbar")))

(ert-deftest helpful--format-reference ()
  (should
   (equal
    (helpful--format-reference '(def foo) 10 1 123 "/foo/bar.el")
    "(def foo ...) 1 reference"))
  (should
   (equal
    (helpful--format-reference '(advice-add 'bar) 10 1 123 "/foo/bar.el")
    "(advice-add 'bar ...) 1 reference")))

(ert-deftest helpful--format-docstring ()
  "Ensure we create links in docstrings."
  ;; If it's bound, we should link it.
  (let* ((formatted (helpful--format-docstring "foo `message'."))
         (m-position (s-index-of "m" formatted)))
    (should (get-text-property m-position 'button formatted)))
  ;; If it's not bound, we should not.
  (let* ((formatted (helpful--format-docstring "foo `messagexxx'."))
         (m-position (s-index-of "m" formatted)))
    (should (not (get-text-property m-position 'button formatted)))
    ;; But we should always remove the backticks.
    (should (equal formatted "foo messagexxx."))))

(setq helpful-var-without-defvar 'foo)

(ert-deftest helpful--definition ()
  ;; Ensure we don't crash on calling `helpful--definition' on
  ;; variables defined without `defvar'.
  (helpful--definition 'helpful-var-without-defvar nil)
  ;; Handle definitions of variables in C source code.
  (let* ((emacs-src-path (f-join default-directory "emacs-25.3" "src")))
    (if (f-exists-p emacs-src-path)
        (let ((find-function-C-source-directory emacs-src-path))
          (helpful--definition 'default-directory nil))
      (message "No Emacs source code found at %S, skipping test. Run ./download_emacs_src.sh"
               emacs-src-path))))

(ert-deftest helpful-variable ()
  "Smoke test for `helpful-variable'."
  (helpful-variable 'tab-width))

(ert-deftest helpful--signature ()
  "Ensure that autoloaded functions are handled gracefully"
  (should
   (equal (helpful--signature 'some-unused-function)
          "(some-unused-function [Arg list not available until function definition is loaded.])")))

(ert-deftest helpful-function--single-buffer ()
  "Ensure that calling `helpful-buffer' does not leave any extra
buffers lying around."
  (let ((initial-buffers (buffer-list))
        expected-buffers results-buffer)
    (helpful-function #'enable-theme)
    (setq results-buffer (get-buffer "*helpful command: enable-theme*"))
    (setq expected-buffers
          (cons results-buffer
                initial-buffers))
    (should
     (null
      (-difference (buffer-list) expected-buffers)))))

(ert-deftest helpful--kind-name ()
  (should
   (equal
    (helpful--kind-name 'message nil)
    "variable"))
  (should
   (equal
    (helpful--kind-name 'message t)
    "function"))
  (should
   (equal
    (helpful--kind-name 'save-excursion t)
    "special form")))

(ert-deftest helpful--pretty-print ()
  ;; Strings should be formatted with double-quotes.
  (should (equal "\"foo\"" (helpful--pretty-print "foo")))
  ;; Don't crash on large plists using keywords.
  (helpful--pretty-print
   '(:foo foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo :bar bar)))

(ert-deftest helpful-update-after-killing-buf ()
  "If we originally looked at a variable in a specific buffer,
and that buffer has been killed, handle it gracefully."
  ;; Don't crash if the underlying buffer has been killed.
  (let (helpful-buf)
    (with-temp-buffer
      (helpful-variable 'tab-width)
      (setq helpful-buf (current-buffer)))
    (with-current-buffer helpful-buf
      (helpful-update))))

(ert-deftest helpful--canonical-symbol ()
  (should
   (eq (helpful--canonical-symbol 'not t)
       'null))
  (should
   (eq (helpful--canonical-symbol 'emacs-bzr-version nil)
       'emacs-repository-version)))

(ert-deftest helpful--aliases ()
  (should
   (equal (helpful--aliases 'null t)
          (list 'not 'null)))
  (should
   (equal (helpful--aliases 'emacs-repository-version nil)
          (list 'emacs-bzr-version 'emacs-repository-version))))
