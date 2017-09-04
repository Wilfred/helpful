# v0.2

Fixed an issue where we didn't find the path for functions defined
interactively using `cl-defstruct`.

Interactively defined functions (e.g. try re-evaluating package.el.gz
and looking at `package-desc-name`) are converted from raw closures to
equivalent defun forms.

We now show the value of variables too.

Added a command `helpful-callable`, which offers both macros and
functions. This should be a drop-in replacement for
`describe-function`.

Added a command `helpful-key`, which offers help on keybindings much
like `describe-key`.

# v0.1

First release.
