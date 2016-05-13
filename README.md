# .emacs.d

My personal Emacs configuration, reworked and cleaned up using [use-package](https://github.com/jwiegley/use-package) for third-party package management and setup.

Configuration is split up into modules which can be enabled or disabled in the `core/sm-modules.el` file.

Setup
-----

Clone this repository into your home directory as .emacs.d.

    git clone git://github.com/sjrmanning/.emacs.d.git ~/.emacs.d

...then run Emacs. On the first run, Emacs will install and compile any packages handled by the package manager. This is checked on each run, so if you want to add a package to install, simply add it to the list in `init-packages.el`, and the next time Emacs runs it will automatically install and compile the new package alone.

Prerequisites
-------------

Emacs 24+ is needed for package.el and the deftheme color-theme system. This setup is used and tested with 24.5. Starting to play around with Emacs 25, so hopefully 25.x related changes aren’t breaking 24.5 at all.

External requirements
--------------

There are a few cool external tools, mainly around code intelligence, syntax checking and auto-completion, that make this setup work a lot better, but obviously they need to be installed to work.

#### `Editor-wide`
* `brew install the_silver_searcher` for `ag.el` search via `C-s`.
* `brew install editorconfig` for parsing .editorconfig files in projects.
* `brew install aspell --with-lang-en` for spell-checking in writing/commit/text modes.

#### `JavaScript`
* `npm install -g tern` for code-analysis and completion via `tern`.

#### `Ruby`
* `gem install rubocop` for style-checking via flycheck.

#### `Python`
* Install with `pip install`.
* `jedi` and `service_factory` for company intelligence and completion.
* `flake8` for pep8 syntax checking via flycheck.

#### `C#`
* [OmniSharp](http://www.omnisharp.net) (see link for details.)

#### Java
* `brew install global` for the GNU global tag system. Used by [ggtags](https://github.com/leoliu/ggtags).

#### `Markdown`
* `brew install` either `discount` or `markdown` for previewing and compiling markdown documents with `markdown-mode`.
