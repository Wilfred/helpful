# v0.5

Allow function tracing to be enabled/disabled from Helpful buffers.

Ensure docstring references to Info nodes are converted to buttons.

Helpful now shows all aliases for callables and variables, and
highlights which aliases are obsolete.

# v0.4

You can now enable edebug directly from helpful buffers!

Variables may be set from the helpful buffer, and booleans can be
toggled. If a variable is a `defcustom`, we also offer Customize.

Show the name of variables at the top of the helpful buffer.

Improved handling of special forms, and prevent users from
accidentally unbinding special forms.

Ensure we can find (and jump to) the definition of functions even if the source
buffer has narrowing in effect.

Fixed an issue where calling helpful commands on interactively defined
functions would overwrite unrelated buffers. Fixed a crash with
interactively defined functions being edebugged.

Added a cleanup command `helpful-kill-buffers`.

Fixed an issue with helpful making recentf write broken paths to
~/.recentf, breaking Emacs startup.

# v0.3

Fixed a crash on autoloaded functions that aren't loaded yet.

Fixed a crash in `helpful-key` for symbols that aren't bound to
commands.

Fixed an issue where viewing help for a function opened a buffer with
its source code. Helpful now cleans up any extra buffers it created.

Pressing RET on the extracted source code now goes to the buffer and
position where the source code is located.

TAB now moves between buttons in helpful buffers.

Buffer names now include 'function' or 'variable' etc, e.g. `*helpful
function: message*`.

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
