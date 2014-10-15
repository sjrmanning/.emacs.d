;;; New Emacs init.

;; Prepare paths.
(add-to-list 'load-path (expand-file-name "init/" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

(require 'init-look)
(require 'init-defaults)
(require 'init-editing)
(require 'init-defuns)
(require 'init-bindings)

;; Ensure required packages are installed and configured.
(require 'init-packages)

(require 'init-personal)
