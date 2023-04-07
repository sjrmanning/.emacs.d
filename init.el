;;; init.el --- Start of the Emacs initialisation process.

;; Prepare paths.
(add-to-list 'load-path (locate-user-emacs-file "core"))
(add-to-list 'load-path (locate-user-emacs-file "modules"))

;; Set up some sane defaults.
(require 'sm-defaults)

;; Custom functions.
(require 'sm-defuns)

;; Personal settings unique to my setup.
(require 'sm-personal)

;; Set up and load available modules.
(require 'sm-modules)
