;;; init.el --- Start of the Emacs initialisation process.

;; Prepare paths.
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set up some sane defaults.
(require 'sm-defaults)

;; Custom functions.
(require 'sm-defuns)

;; Set up available modules and the `load-modules' function.
(require 'sm-modules)

;; Personal settings unique to my setup.
(require 'sm-personal)

;; Load configured modules.
(sm/load-modules)
