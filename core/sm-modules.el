;;; sm-modules.el --- Configures available modules and the package manager.

;; Set up the package manager.
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Ensure `use-package' is installed.
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq sm/modules
      '(sm-path
        sm-editing
        sm-ui
        sm-appearance
        sm-snippets
        sm-searching
        sm-navigation
        sm-company
        sm-writing
        sm-projects
        sm-vc
        sm-org
        sm-coding-general
        sm-coding-c
        sm-coding-csharp
        sm-coding-elixir
        sm-coding-java
        sm-coding-js
        sm-coding-python
        sm-coding-ruby
        sm-coding-swift))

(defun sm/load-modules ()
  (interactive)
  (dolist (module sm/modules) (require module)))

(provide 'sm-modules)
