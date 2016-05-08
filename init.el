;; Init
;; Uses `use-package' extensively to configure packages.

;; Prepare paths.
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Set up some sane defaults.
(require 'init-defaults)

;; Custom functions.
(require 'init-defuns)

;; Set up appearance.
(require 'init-look)

;; Editing specific settings.
(require 'init-editing)

;; Custom key-bindings separate from third-party package binds.
(require 'init-bindings)

;; Ensure required packages are installed and configured.
(require 'init-packages)

;; Org-mode configuration in its own file.
(require 'init-org)

;; Personal settings unique to my setup.
(require 'init-personal)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
