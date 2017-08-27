# Helpful [![Build Status](https://travis-ci.org/Wilfred/helpful.svg?branch=master)](https://travis-ci.org/Wilfred/helpful) [![Coverage Status](https://coveralls.io/repos/github/Wilfred/helpful/badge.svg?branch=master)](https://coveralls.io/github/Wilfred/helpful?branch=master) [![MELPA](http://melpa.org/packages/helpful-badge.svg)](http://melpa.org/#/helpful) [![](https://tokei.rs/b1/github/wilfred/helpful)](https://github.com/Aaronepower/tokei)

Helpful is an alternative to the built-in Emacs help that provides
much more contextual information.

![screenshot](screenshots/helpful.png)

## Features

### Source code

![screenshot](screenshots/helpful_source.png)

Helpful will try really hard to show the source code. It shows the
source code for interactively defined functions (unlike the built-in
Help) and falls back to the raw sexp if no source is available.

### View Callers

![screenshot](screenshots/helpful_refs.png)

Helpful will show you where a function is being called!

### Prettier Docstrings

![screenshot](screenshots/helpful_docstring.png)

Docstrings in helpful:

* Highlight the summary (the first sentence)
* Include cross-references
* Hide superfluous puncuation

### Symbol Properties

![screenshot](screenshots/helpful_props.png)

Helpful will show you the properties that have been applied to the
current symbol. This provides visibility of features
like edebug or byte-code optimisation.

### Describe Commands

Helpful provides a separate `helpful-command` function, for when you
just want to view interactive functions.

### View Keymaps

![screenshot](screenshots/helpful_bindings.png)

Helpful displays any keybindings that apply to interactive functions.

### Integrated Tooling

![screenshot](screenshots/helpful_tools.png)

You can trace, debug (coming soon) or disassemble functions from inside
Helpful. This is discoverable and doesn't require memorisation of
commands.

## Inspirations

This project has been heavily influenced by:

* [help+.el](https://www.emacswiki.org/emacs/help+.el), help-fns+.el, help-mode+.el
* [Dave Williams' demo of Lucid's Energize](https://www.youtube.com/watch?v=pQQTScuApWk)
