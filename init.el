;; Init
;; Uses `use-package' extensively to configure packages.

;; Prepare paths.
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Set up appearance.
(require 'init-look)

;; Set up some sane defaults.
(require 'init-defaults)

;; Editing specific settings.
(require 'init-editing)

;; Custom functions.
(require 'init-defuns)

;; Custom key-bindings separate from third-party package binds.
(require 'init-bindings)

;; Ensure required packages are installed and configured.
(require 'init-packages)

;; Personal settings unique to my setup.
(require 'init-personal)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
