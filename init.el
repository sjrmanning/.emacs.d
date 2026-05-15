;;; init.el --- Start of the Emacs initialisation process.  -*- lexical-binding: t; -*-

;; restore file-name-handler-alist.  I restore this variable at the beginning,
;; as some package may add something to the list.
(setq file-name-handler-alist sm/file-name-handler-alist-orig)

;; Prepare paths.
(add-to-list 'load-path (locate-user-emacs-file "core"))
(add-to-list 'load-path (locate-user-emacs-file "modules"))
(add-to-list 'load-path (locate-user-emacs-file "etc/built-modules"))

(add-to-list 'exec-path "/opt/homebrew/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Library/Frameworks/Python.framework/Versions/3.9/bin")

;; Set up some sane defaults.
(require 'sm-defaults)

;; Custom functions.
(require 'sm-defuns)

;; Personal settings unique to my setup.
(require 'sm-personal)

;; Set up and load available modules.
(require 'sm-modules)
