# .emacs.d

My personal Emacs configuration, using [straight.el](https://github.com/raxod502/straight.el) and [use-package](https://github.com/jwiegley/use-package) for third-party package management and setup. Straight in particular allows for some extreme optimization — current `emacs-init-time` on an M1 Max is ~170ms for terminal and under 300ms for GUI! ⚡️

Configuration is split up into modules which can be enabled or disabled in the `core/sm-modules.el` file. Personal settings such as note-taking directories are kept in `core/sm-personal.el`. Anything private can be placed in `~/.private/elisp/` — files placed there will be loaded during initialization.

Setup
-----

Clone this repository into your home directory as .emacs.d.

    git clone git://github.com/sjrmanning/.emacs.d.git ~/.emacs.d

...then run Emacs. On the first run, Emacs will install and compile any packages handled by the package manager, including native-compilation via libgccjit if supported.

Emacs version
-------------

I’m using native-compilation (`--with-native-comp`) and Emacs 29 with treesitter support, so there is some configuration using `-ts-mode` for some major modes.

Fonts and Theme
---------------

Fonts used are defined in `core/sm-appearance.el`. A custom version of Solarized (light) is used by default, but theme can also be changed in the `sm-appearance.el` file.

External requirements
--------------

There are a few cool external tools, mainly around code intelligence, syntax checking and auto-completion, that make this setup work a lot better, but obviously they need to be installed to work.

#### `Editor-wide`
* `brew install ripgrep` for `rg` for blazing fast search via `C-s` and `C-S`.
* `brew install editorconfig` for parsing .editorconfig files in projects.
* `brew install aspell --with-lang-en` for spell-checking in writing/commit/text modes.

#### `Language Servers`
* This config uses the built-in (Emacs 29+) [`eglot`](https://github.com/joaotavora/eglot) to provide auto-completion and other code intelligence features for various programming languages. Pre-reqs differ by language, and some will handle their prerequisites themselves (`lsp-python-ms` for example will configure its own server).

    A list of available language server implementations can be found [here](https://microsoft.github.io/language-server-protocol/implementors/servers/).

#### `Terminals`
* `vterm` requires some compilation on first-use. You’ll need `cmake` and `libvterm`. Both can be installed with `brew`.

#### `Ruby`
* `gem install rubocop` for style-checking via flycheck.

#### `Markdown`
* There are a few options for previewing and compiling Markdown documents. I’m currently using [pandoc](https://pandoc.org/index.html), installed via `brew install pandoc`. You can also use `discount` or `markdown`, you’ll just need to update `markdown-command`.
