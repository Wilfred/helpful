(require 'ert)
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

(ert-deftest helpful--no-symbol-properties ()
  "Helpful should handle functions without any symbol properties."
  ;; Interactively evaluating this file will set edebug properties on
  ;; test-foo, so remove all properties.
  (setplist #'test-foo-no-properties nil)

  (should (helpful-function #'test-foo-no-properties)))

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
  "Ensure we don't crash on calling `helpful--definition' on
variables defined without `defvar'."
  (helpful--definition 'helpful-var-without-defvar nil))

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
    (setq results-buffer (get-buffer "*helpful: enable-theme*"))
    (setq expected-buffers
          (cons results-buffer
                initial-buffers))
    (should
     (null
      (-difference (buffer-list) expected-buffers)))))
