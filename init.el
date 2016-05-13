;;; init.el --- Start of the Emacs initialisation process.

;; Increase the GC threshold as soon as possible.
(setq gc-cons-threshold 50000000)

;; Packages need to be initialised in init.el in Emacs 25.x.
(unless (fboundp 'package-initialize)
  (require 'package))
(package-initialize)

;; Prepare paths.
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

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
