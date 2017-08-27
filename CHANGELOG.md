# v0.2

Fixed an issue where we didn't find the path for functions defined
interactively using `cl-defstruct`.

Interactively defined functions (e.g. try re-evaluating package.el.gz
and looking at `package-desc-name`) are converted from raw closures to
equivalent defun forms.

# v0.1

First release.
