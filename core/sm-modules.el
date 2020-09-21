;;; sm-modules.el --- Configures available modules and the package manager.

;; straight.el tuning
(setq straight-cache-autoloads t)
(setq straight-check-for-modifications '(check-on-save))

;; Bootstrap straight.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use `use-package' via straight.el
;; See raxod502/straight.el issue #330
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

;; Hook up use-package's support for chords (simultaneous pairs and/or
;; rapid pairs of keystrokes).  This obviates explicitly loading the
;; keychord package.
(use-package use-package-chords
  :demand
  :config (key-chord-mode 1))

;; Install delight as required by `:delight' with use-package.
(use-package delight)

(setq sm/modules
      '(
        sm-keybindings
        sm-path
        sm-editing
        sm-ui
        sm-appearance
        sm-searching
        sm-snippets
        sm-navigation
        sm-company
        sm-writing
        sm-projects
        sm-source-control
        ;; sm-org
        sm-coding-general
        sm-coding-groovy
        sm-coding-cc
        ;; sm-coding-csharp
        sm-coding-elixir
        sm-coding-go
        sm-coding-java
        sm-coding-js
        sm-coding-python
        sm-coding-ruby
        sm-coding-scad
        ;; sm-coding-swift
        sm-coding-web
        sm-coding-perl
        sm-coding-sh
        sm-coding-stats
        sm-coding-hcl
        sm-dired
        sm-mail
        sm-data
        sm-terraform
        sm-coding-nextflow
        ))

(defun sm/load-modules ()
  (interactive)
  (dolist (module sm/modules) (require module)))

(provide 'sm-modules)
