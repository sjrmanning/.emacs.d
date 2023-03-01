;;; sm-modules.el --- Configures available modules and the package manager.

;; straight.el tuning
(setq straight-cache-autoloads t)
(setq straight-check-for-modifications '(check-on-save))

;; Temporarily using the develop branch which supports native-compilation.
(setq straight-repository-branch "develop")

;; Bootstrap straight.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use `use-package' via straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Defer by default. Most packages should be configured with `:mode' or similar
;; but this is a bit safer and sped up init-time.
;; For cases where `:mode' etc. don't make sense, `:defer nil' explicitly.
(setq use-package-always-defer t)

;; Install delight as required by `:delight' with use-package.
(use-package delight)

(setq sm/modules
      '(sm-appearance
        sm-coding-cc
       ;sm-coding-elixir
        sm-coding-general
        sm-coding-go
        sm-coding-java
        sm-coding-js
        sm-coding-python
        sm-coding-ruby
        sm-coding-swift
        sm-coding-web
        sm-complete
        sm-editing
        sm-navigation
        sm-notes
        sm-path
        sm-projects
        sm-searching
        sm-snippets
        sm-source-control
        sm-ui
        sm-utils
        sm-writing))

(defun sm/load-modules ()
  (interactive)
  (dolist (module sm/modules) (require module)))

(provide 'sm-modules)
