;;; init.el --- Start of the Emacs initialisation process.

;; Prepare paths.
(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Set up some sane defaults.
(require 'sm-defaults)

;; Custom functions.
(require 'sm-defuns)

;; Custom key-bindings separate from third-party package binds.
(require 'sm-bindings)

;; Set up available modules and `load-modules' function.
(require 'sm-modules)

;; Personal settings unique to my setup.
(require 'sm-personal)

;; Load configured modules.
(sm/load-modules)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
