# `.emacs.d`

My personal Emacs configuration, reworked and cleaned up using [use-package](https://github.com/jwiegley/use-package) for third-party package management and setup.

Setup
-----

Clone this repository into your home directory as .emacs.d.

    git clone git://github.com/stafu/.emacs.d.git ~/.emacs.d

...then run Emacs. On the first run, Emacs will install and compile any packages handled by the package manager. This is checked on each run, so if you want to add a package to install, simply add it to the list in `init-packages.el`, and the next time Emacs runs it will automatically install and compile the new package alone.

Prerequisites
-------------

Emacs 24+ is needed for package.el and the deftheme color-theme system. This setup is used and tested with 24.4.
