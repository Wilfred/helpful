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

