# .emacs.d

My personal Emacs configuration, using [straight.el](https://github.com/raxod502/straight.el) and [use-package](https://github.com/jwiegley/use-package) for third-party package management and setup. Straight in particular allows for some extreme optimization — current init time on a 2019 MacBook Pro is under 0.3 seconds for terminal and under 0.5 seconds for GUI! ⚡️

Configuration is split up into modules which can be enabled or disabled in the `core/sm-modules.el` file. Personal settings such as note-taking directories are kept in `core/sm-personal.el`. Anything private can be placed in `~/.private/elisp/` — files placed there will be loaded during initialization.

Setup
-----

Clone this repository into your home directory as .emacs.d.

    git clone git://github.com/sjrmanning/.emacs.d.git ~/.emacs.d

...then run Emacs. On the first run, Emacs will install and compile any packages handled by the package manager, including native-compilation via libgccjit if supported. This is checked on each run, so if you want to add a package to install, simply add it to the list in `init-packages.el`, and the next time Emacs runs it will automatically install and compile the new package alone.

Emacs version
-------------

I’m using [gccemacs](https://akrl.sdf.org/gccemacs.html) on macOS, so your mileage may vary, but I believe any recent enough Emacs above version 25 should work fine.

Fonts
-----

Fonts used are defined in `core/sm-appearance.el`. [Offlig](https://github.com/sjrmanning/Offlig) and [iA Writer](https://github.com/iaolo/iA-Fonts) are used by default.

External requirements
--------------

There are a few cool external tools, mainly around code intelligence, syntax checking and auto-completion, that make this setup work a lot better, but obviously they need to be installed to work.

#### `Editor-wide`
* `brew install ripgrep` for `rg` for blazing fast search via `C-s` and `C-S`.
* `brew install editorconfig` for parsing .editorconfig files in projects.
* `brew install aspell --with-lang-en` for spell-checking in writing/commit/text modes.

#### `Language Servers`
* This config uses [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode) to provide auto-completion and other code intelligence features for various programming languages. Pre-reqs differ by language, and some will handle their prerequisites themselves (`lsp-python-ms` for example will configure its own server).

    A list of available language server implementations can be found [here](https://microsoft.github.io/language-server-protocol/implementors/servers/).

#### `Terminals`
* `vterm` requires some compilation on first-use. You’ll need `cmake` and `libvterm`. Both can be installed with `brew`.

#### `Ruby`
* `gem install rubocop` for style-checking via flycheck.

#### `Markdown`
* There are a few options for previewing and compiling Markdown documents. I’m currently using [pandoc](https://pandoc.org/index.html), installed via `brew install pandoc`. You can also use `discount` or `markdown`, you’ll just need to update `markdown-command`.
