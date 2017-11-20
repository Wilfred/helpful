# v0.3

Fixed a crash on autoloaded functions that aren't loaded yet.

Fixed a crash in `helpful-key` for symbols that aren't bound to
commands.

Fixed an issue where viewing help for a function opened a buffer with
its source code. Helpful now cleans up any extra buffers it created.

Pressing RET on the extracted source code now goes to the buffer and
position where the source code is located.

# v0.2

Fixed a crash on viewing aliased primitive functions.

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

Added a command `helpful-symbol`, which offers help on variables,
functions and macros. It prompts the user if a symbol is both a
variable and a callable.

# v0.1

First release.
