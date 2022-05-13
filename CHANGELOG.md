# v0.20 (unreleased)

# v0.19 (released 12th May 2022)

Fixed a hang when looking at functions that had advice but hadn't yet
been loaded by the autoloader (#179, #191).

Fixed issue with displaying circular data structures.

Fixed a crash in `helpful-variable` in files that weren't
syntactically valid lisp.

Fixed stack overflow in macroexpanding large s-expressions (#279).

# v0.18

Show the original value for custom variables whose value has changed.

Report the package version when custom variables were added.

Fixed a crash on assigning byte-compiled objects to keybindings.

Fixed an issue with advice being shown in docstrings on Emacs 27+.

Symbol links in docstrings are now smarter in cases like "function
`foo'".

## Improvements to the default symbol offered

Functions:

* If the symbol at point is a bound function, offer that.
* If point is at a function call, offer that.

Variables:

* If the symbol at point is a bound variable, offer that.
* If point is at a `defvar` or `defcustom` call, offer that variable.

Symbols:

* If the symbol at point is a bound function, offer that.
* Try the function, then the variable heuristics described above.

This should also make transitioning from help.el easier, and should
improve helpful in literate org-mode files.

# v0.17

Fixed a minor docstring formatting issue when keymap references were
on their own line.

Symbols of the form `foo-functions` (e.g. `after-change-functions`)
are now rendered as hooks.

String literals are now rendered correctly in docstrings. Previously
command substitution applied inside literals.

Fixed a crash on keyboard macros in keymaps.

String values are now shown as literals by default.

Fixed an issue with modes that override after-change-major-mode-hook,
such as `global-hi-lock-mode`.

Added bookmark support.

# v0.16

Improved wording when looking at aliases.

Fixed several issues for symbols that contain spaces.

Fixed an issue when viewing `inhibit-read-only`.

Improved the buffer prompt to be more relevant when inspecting
buffer-local values.

Better source detection for functions defined by `defstruct` or other
macros.

Fixed issues with functions that had more than one advice active.

Added looking up C-style Lisp names.

Set `comment-start` inside helpful buffers, to fix external packages
relying on that variable.

Helpful now always autoloads callables if they aren't already
loaded. This is consistent with help.el (unless you've overridden
`help-enable-auto-load`), produces more useful results, and fixes
crashes rendering some docstrings.

# v0.15

Fixed a crash on formatting values.

# v0.14

Named keyboard macros are now supported.

Function callees are now sorted.

Fixed an issue where source code of primitive variables included the
whole surrounding function. This was less useful and significantly
slower.

Fixed an issue with Remacs compatibility.

Fixed an issue with the prompt when setting variables whose current
value was nil or a keyword.

Fixed an issue with going to definitions when the source buffer was
narrowed.

Navigation keybindings now work in callee list buffers.

Fixed an issue where we didn't show function aliases as aliases when
they pointed to a primitive function.

Fixed an issue with primitive variables when using Helpful with
Remacs.

# v0.13

Buffer-local variables are now highlighted, and it's possible to see
all the different values of a buffer-local variable.

Variable values are now shown before docstrings.

Fixed an issue where special forms were incorrectly described as
functions.

Helpful now shows keybindings for aliases of the current command too.

Fixed an issue where functions defined in .el.gz files were not
recognised as being autoloaded.

Fixed an issue where we didn't show the source code for advised
primitives.

Show the default value for the symbol in the minibuffer prompt.

# v0.12

Added a 'pretty view' for string values, keymap values, and hooks.

* For strings, we show properties natively in Emacs.

* For keymaps, we render each keybinding in a human-readable way along
with a link to the relevant command.

* For hooks, which are lists of functions and symbols, convert symbols
  to links.

Added a 'view callees' button which shows functions called inside the
bodies of source code, with links to view them inside helpful.

Fixed a crash on keymaps where keys are bound to anonymous functions.

Improved performance (PR by @nickdrozd).

It's now possible to move between buttons in the Helpful buffer with
`n` and `p`.

# v0.11

Further work on syntax highlighting performance for large code
snippets. Helpful now informs the user when it has intentionally
disabled highlighting, and shows a link to the relevant setting.

Fixed an issue where Helpful didn't find keybindings if the command
was only referenced in `minor-mode-map-alist`.

Fixed a crash when minor modes had invalid keymaps.

Fixed an issue with `helpful-variable` where the user was pointlessly
prompted about file variables.

Fixed a crash when a docstring referenced a non-existent keymap.

Linkify docstring URLs of the form `<http://foo>`.

# v0.10

When visiting a reference, the occurrences of the symbol are
temporarily highlighted.

Keybindings in `widget-global-map` are now ignored as they're rarely
informative.

URLs in docstrings are now converted to buttons.

Function signatures are now correct when a function uses
`(declare (advertised-calling-convention ...))`.

Fixed an issue where escaped backticks were confused with unescaped
backticks in docstrings.

Fixed an issue with very large code snippets making helpful hang due
to slow syntax highlighting. This was particularly problematic for C
functions and variables.

When extracting the source for an item, include preceding comments and
autoload cookies.

# v0.9

Much better handling of aliases: show aliases differently to their
underlying symbol, show the `defalias` call in the source code, don't
show duplicates in the Aliases list, and cross-reference the Elisp
manual.

Fixed an issue where keymaps with prefix keys were not rendered
correctly.

Fixed an issue when looking at variable docs where we would visit the
definition buffer without the user requesting it.

Wording polish for finding references of primitives.

Fixed some corner cases in references to info manual sections not
being linkified.

Fixed an issue where the definition of interactively defined functions
wasn't shown.

Pretty-printing is now much more robust, gracefully handling very
large lists.

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

Ensure we can find (and jump to) the definition of functions even if
the source buffer has narrowing in effect.

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

If a symbol is mentioned in the Emacs manual, show a link to the
relevant section.

# v0.1

First release.
