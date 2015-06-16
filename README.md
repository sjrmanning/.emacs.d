# .emacs.d

My personal Emacs configuration, reworked and cleaned up using [use-package](https://github.com/jwiegley/use-package) for third-party package management and setup.

Setup
-----

Clone this repository into your home directory as .emacs.d.

    git clone git://github.com/stafu/.emacs.d.git ~/.emacs.d

...then run Emacs. On the first run, Emacs will install and compile any packages handled by the package manager. This is checked on each run, so if you want to add a package to install, simply add it to the list in `init-packages.el`, and the next time Emacs runs it will automatically install and compile the new package alone.

Prerequisites
-------------

Emacs 24+ is needed for package.el and the deftheme color-theme system. This setup is used and tested with 24.5.

External requirements
--------------

There are a few cool external tools, mainly around code intelligence, syntax checking and auto-completion, that make this setup work a lot better, but obviously they need to be installed to work.

#### Editor-wide
* `brew install the_silver_searcher` for `ag.el` search via `C-s`.
* `brew install editorconfig` for parsing .editorconfig files in projects.

#### Javascript
* `npm install -g tern` for code-analysis and completion via `tern`.

#### Python
* Install with `pip install`.
* `jedi` and `service_factory` for company intelligence and completion.
* `flake8` for pep8 syntax checking via flycheck.

#### C#
* [OmniSharp](http://www.omnisharp.net) (see link for details.)

#### Markdown
* `brew install` either `discount` or `markdown` for previewing and compiling markdown documents with `markdown-mode`.
