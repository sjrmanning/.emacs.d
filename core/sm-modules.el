;;; sm-modules.el --- Configures available modules and the package manager.

;; straight.el pre-bootstrap settings.
(setq-default straight-cache-autoloads t)
(setq-default straight-check-for-modifications nil)
(setq-default straight-repository-branch "develop")

;; Bootstrap straight.
(defvar bootstrap-version)
(let ((bootstrap-file
       (locate-user-emacs-file "straight/repos/straight.el/bootstrap.el"))
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
(setq-default straight-use-package-by-default t)

;; Defer by default. Most packages should be configured with `:mode' or similar
;; but this is a bit safer and sped up init-time.
;; For cases where `:mode' etc. don't make sense, `:defer nil' explicitly.
(setq-default use-package-always-defer t
              use-package-expand-minimally t)

;; Install diminish as required by `:diminish' with use-package.
;; Note: I don't use anything special here so no need for `:delight',
;; which is slightly heavier than diminish.
(use-package diminish)

(defvar sm/modules
  '(sm-appearance
    sm-coding-general
    sm-coding-go
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

(dolist (module sm/modules) (require module))

(provide 'sm-modules)
