(require 'ert)
(require 'helpful)

(defun test-foo ()
  "Docstring here."
  nil)

(defun test-foo-advised ()
  "Docstring here too."
  nil)

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
    (helpful--docstring #'test-foo)
    "Docstring here.")))

(ert-deftest helpful--docstring-advice ()
  "Get the docstring on advised functions."
  (should
   (equal
    (helpful--docstring #'test-foo-advised)
    "Docstring here too.")))

(defun test-foo-no-docstring ()
  nil)

(ert-deftest helpful--no-docstring ()
  "We should not crash on a function without a docstring."
  (should (null (helpful--docstring #'test-foo-no-docstring))))

(defun test-foo-usage-docstring ()
  "\n\n(fn &rest ARGS)"
  nil)

(ert-deftest helpful--usage-docstring ()
  "If a function docstring only has usage, do not return it."
  (should (null (helpful--docstring #'test-foo-usage-docstring))))

