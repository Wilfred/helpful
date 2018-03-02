# v0.8

Added setting `helpful-switch-buffer-function` to allow users to
control how the Helpful buffer is shown.

Fixed a crash on functions where the last form in their definition is
a plain symbol.

Fixed an issue where we would show function source code for variables
where we can't find the variable definition.

Fixed a crash on functions defined in a buffer that isn't backed by a
file. We now provide a button to navigate to that buffer.

Improved wording for functions with no source at all.

Fixed an issue with obsolete aliases without version information.

Fixed an issue with top-level references where we would show the
previous form rather than the relevant one.

# v0.7

Helpful buffers now start with a summary of what you're looking at,
including quick links to the relevant source code. Interactive and
autoloaded functions are highlighted and include relevant links to the
manual.

Fixed a crash on functions defined in a .elc when the .el file is not
available.

Fixed some crashes on primitive functions.

Improved finding source code when edebug info is available.

Better handling of docstrings:

* All strings in quotes are highlighted. For example, previously
  `` `C-M-\' `` was not highlighted because `C-M-\` doesn't look like
  a symbol.
* Handle nested key sequences correctly, such as `` `\\[foo]' ``.
* Handle keymap sequences, such as `\\{foo-mode-map}`.

# v0.6

Added imenu support! You can now navigate between headings with imenu.

Better handling of docstrings:

* Correctly handle \= escapes
* Quoted keywords are now highlighted
* Correctly highlight docstrings that contain standalone `` ` ``
* Ensure we link command references in variable docstrings too

Added disassemble buttons to byte-code functions in symbol properties.

Fixed an issue with the prompt when setting variables to symbol
values.

Smarter handling of keybindings:

* Consider all keymaps, not just `foo-mode-map`.
* Show keybindings where they are defined, don't show them in keymaps
  that inherit them.
* Don't show menu bar items, as they clutter the display (please file
  a bug if you miss this).
* Correctly handle `\<foo-map>` in docstrings.

When `helpful-max-buffers` is set, we now kill buffers in the order of
least recently accessed. Previously, we killed in creation order.

If a function also has a unit test of the same name, allow it to be
run directly from the property list.

Fixed an issue where we would repeatedly prompt the user regarding
unsafe buffer-local variables.

# v0.5

Allow function tracing to be enabled/disabled from Helpful buffers.

Ensure docstring references to Info nodes are converted to
buttons. Docstring references to command keys are converted to buttons
too (see `set-mark-command` for an example).

Helpful now shows all aliases for callables and variables, and
highlights which aliases are obsolete.

Improved helpful performance for primitives when Emacs source code is
loaded.

Helpful will now only keep the last 5 buffers, to avoid cluttering
your buffer list. You can customise this behaviour with
`helpful-max-buffers`: set it to 1 to cleanup all previous buffers, or
set it to `nil` never cleanup buffers.

## recentf bug

Helpful had an issue where it would call find-file with propertized
strings. This broke various recentf features.

This has been fixed, and you can check if you're running a fixed
version by seeing whether you have a `helpful--button` function
defined. If you do, your version is new enough.

You will aso need to edit your `~/.emacs.d/recentf` and
`recentf-save.el` to remove any lines that start with a `#`:

``` emacs-lisp
#("/usr/share/emacs/25.3.50/lisp/frame.el.gz" 0 41 (button (t) category helpful-navigate-button-button path #0 position 2815))
```

Otherwise, you will get `Invalid read syntax: "#"` when starting
Emacs.

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
