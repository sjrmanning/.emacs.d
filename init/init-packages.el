;; File: init-packages.el
;; Installs and configures packages, including third-party packages
;; which will be downloaded when required via `use-package'.

;; Required for running this code.
(require 'init-defuns)
(require 'package)

;; Add the Melpa repository to the list of package sources.
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initialise the package system.
(package-initialize)

;; Ensure use-package is installed.
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; exec-path-from-shell
;; Use $PATH from user's shell in Emacs.
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system (quote (mac ns)))
  :defer 2
  :config (exec-path-from-shell-initialize))

;; yasnippet
(use-package yasnippet
  :ensure t
  :defer 2
  :init
  ;; Ensure custom snippets dir exists.
  (defvar custom-snippets-dir (sm/emacs.d "etc/snippets/"))
  (sm/mkdir-p custom-snippets-dir)
  ;; Replace default custom dir with our own.
  (setq yas-snippet-dirs '(custom-snippets-dir
                           yas-installed-snippets-dir))
  ;; Suppress excessive log messages
  (setq yas-verbosity 1)
  :config
  (yas-global-mode t)
  ;; Disable yasnippet in some modes.
  (defun yas-disable-hook ()
    (setq yas-dont-activate t))
  (add-hook 'term-mode-hook 'yas-disable-hook)
  (add-hook 'comint-mode-hook 'yas-disable-hook)
  (add-hook 'erc-mode-hook 'yas-disable-hook))

;; colors!
(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t)
  (load-theme 'monokai-overrides t))

;; ag (silver surfer)
(use-package ag
  :ensure t
  :bind ("C-c s" . ag)
  :config (setq ag-highlight-search t))

;; anzu
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; company-mode
(use-package company
  :ensure t
  :diminish " ©"
  :commands (company-mode global-company-mode)
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'comint-mode-hook 'company-mode)
  :config
  ;; Quick-help (popup documentation for suggestions).
  (use-package company-quickhelp
    :if window-system
    :ensure t
    :init (company-quickhelp-mode 1))
  ;; Company settings.
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.25)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq company-backends
        (mapcar #'sm/backend-with-yas company-backends)))

;; editorconfig and conf-mode setup.
(use-package editorconfig
  :ensure t)
(use-package conf-mode
  :mode (("\\.editorconfig$" . conf-mode)
         ("\\.conf" . conf-mode)
         ("\\.cfg" . conf-mode)
         ("\\.ini" . conf-mode)))

;; typo
;; Mode for typographical editing.
(use-package typo
  :ensure t
  :commands (typo-mode)
  :init (add-hook 'text-mode-hook 'typo-mode))

;; flyspell spell checking.
(use-package flyspell
  :diminish flyspell-mode
  :commands (flyspell-mode)
  :init (add-hook 'text-mode-hook 'flyspell-mode))

;; org-mode
(use-package org
  :ensure t
  :commands (org-mode)
  :config
  (setq org-src-fontify-natively t))

;; deft
(use-package deft
  :ensure t
  :commands (deft)
  :bind ("M-<f1>" . deft)
  :config
  (setq deft-extension "org"
        deft-directory "~/Org/deft/"
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-auto-save-interval 30.0))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :init (setq mc/list-file (sm/emacs.d "etc/.mc-lists.el"))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;; expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; flycheck
(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :diminish " ✓"
  :init (add-hook 'prog-mode-hook 'flycheck-mode))

;; switch-window
;; Provides visual cues to instantly switch on C-x o.
(use-package switch-window
  :ensure t
  :defer t
  :bind ("C-x o" . switch-window))

;; magit and monky
;; Modes for git and mercurial.
(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :commands (magit-status)
  :bind ("C-x g" . magit-status)
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  ;; Full-screen magit status with restore.
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-mode-quit-window (around magit-restore-screen activate)
    ad-do-it
    (jump-to-register :magit-fullscreen))
  ;; Use flyspell during commits.
  (add-hook 'git-commit-mode-hook '(lambda () (flyspell-mode t))))

(use-package monky
  :ensure t
  :commands (monky-status)
  :bind ("C-c g" . monky-status)
  :config
  ;; Similar full-screen config for monky.
  (defadvice monky-status (around monky-fullscreen activate)
    (window-configuration-to-register :monky-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice monky-quit-window (around monky-restore-screen activate)
    ad-do-it
    (jump-to-register :monky-fullscreen))
  ;; Flyspell during commits.
  (add-hook 'monky-log-edit-mode-hook '(lambda () (flyspell-mode t))))

;; git-gutter
(use-package git-gutter
  :ensure t
  :config (use-package git-gutter-fringe :ensure t)
  :init
  (require 'git-gutter-fringe)
  (setq git-gutter:handled-backends '(git hg))
  (global-git-gutter-mode t))

;; smex
(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :init (smex-initialize))

;; ido
(use-package ido
  :ensure t
  :init (ido-mode 1)
  :config
  (use-package flx-ido
    :ensure t)
  (use-package ido-vertical-mode
    :ensure t)
  (use-package ido-ubiquitous
    :ensure t)
  (setq ido-enable-flex-matching t
        ido-enable-prefix nil
        ido-max-prospects 10
        ido-use-faces nil
        flx-ido-use-faces t)
  (ido-everywhere 1)
  (ido-vertical-mode 1)
  (flx-ido-mode 1))

;; avy
(use-package avy
  :ensure t
  :config (setq avy-style 'at)
  :bind (("C-o" . avy-goto-char)
         ("M-g" . avy-goto-line)))

;; markdown
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; smartparens
(use-package smartparens
  :ensure t
  :defer 2
  :diminish " ()"
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)

  ;; sp keybindings.
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
  (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

  (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp))

;; browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :defer t
  :bind ("M-y" . browse-kill-ring))

;; projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-mode projectile-global-mode)
  :bind ("C-c p a" . projectile-ag)
  :init (projectile-global-mode t)
  :config
  ;; Ensure projectile dir exists.
  (defvar my-projectile-dir (sm/emacs.d "cache/projectile"))
  (sm/mkdir-p my-projectile-dir)
  ;; Use projectile dir for cache and bookmarks.
  (let* ((prj-dir (file-name-as-directory my-projectile-dir))
         (prj-cache-file (concat prj-dir "projectile.cache"))
         (prj-bookmarks-file (concat prj-dir "projectile-bkmrks.eld")))
    (setq projectile-cache-file          prj-cache-file
          projectile-known-projects-file prj-bookmarks-file
          projectile-indexing-method     'alien)))

;; perspective
(use-package perspective
  :ensure perspective
  :defer t
  :init (add-hook 'after-init-hook 'persp-mode)
  :config
  (setq persp-initial-frame-name "scratch")
  (defun persp-next ()
    (interactive)
    (when (< (+ 1 (persp-curr-position)) (length (persp-all-names)))
      (persp-switch (nth (1+ (persp-curr-position)) (persp-all-names))))))

;; saveplace
;; Remebers your location in a file when saving files.
(use-package saveplace
  :init
  (setq save-place-file (sm/emacs.d "cache/saveplace"))
  (setq-default save-place t))

;; smooth-scrolling
;; Avoids annoying behaviour when scrolling past the edges of a buffer.
(use-package smooth-scrolling
  :ensure t)

;; whitespace cleanup
;; Automatically cleans whitespace on save.
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode))

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; subword
(use-package subword
  :diminish subword-mode
  :init (global-subword-mode))

;; highlight-numbers
;; Highlights magic numbers in programming modes.
(use-package highlight-numbers
  :ensure t
  :commands (highlight-numbers-mode)
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; rainbow-delimiters
;; Highlights parens, brackets, and braces according to their depth.
(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode))

;; jade.
(use-package jade-mode
  :ensure t
  :mode ("\\.jade$" . jade-mode))

;; javascript
;; js-mode for json.
(use-package javascript-mode
  :mode ("\\.json$" . javascript-mode)
  :init
  (add-hook 'js-mode-hook (lambda () (setq js-indent-level 4))))

;; js2.
(use-package js2-mode
  :ensure t
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter "node"
  :config
  (use-package tern
    :ensure t
    :diminish tern-mode
    :init
    (add-hook 'js2-mode-hook 'tern-mode))
  (use-package company-tern
    :ensure t
    :defer t
    :config (setq company-tern-property-marker " *")
    :init (add-to-list 'company-backends
                       (sm/backend-with-yas 'company-tern)))
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-basic-offset 2))))

;; go
(use-package go-mode
  :ensure t
  :ensure company-go
  :defer t
  :config
  (setq company-go-show-annotation t)
  (add-to-list 'company-backends
               (sm/backend-with-yas 'company-go)))

;; python
(use-package python
  :commands (python-mode)
  :init (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (use-package anaconda-mode
    :ensure t
    :commands (anaconda-mode))
  (use-package company-anaconda
    :ensure t
    :init (add-to-list 'company-backends
                       (sm/backend-with-yas 'company-anaconda))))

;; ruby
(use-package ruby-mode
  :interpreter "ruby"
  :mode (("Fastfile$" . ruby-mode)
         ("Appfile$" . ruby-mode))
  :config
  (use-package ruby-tools :ensure t)
  (use-package inf-ruby :ensure t)
  (add-hook 'ruby-mode-hook
            (lambda ()
              (inf-ruby-minor-mode t)
              (ruby-tools-mode t))))

;; cc-mode/derived modes and hooks
(use-package cc-mode
  :defer t
  :config
  (defun sm/cc-mode-hook ()
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'inexpr-class 0))
  (add-hook 'c-mode-common-hook 'sm/cc-mode-hook)
  (add-hook 'objc-mode-hook
            (lambda ()
              (setq c-basic-offset 4)))
  (add-hook 'java-mode-hook
            (lambda ()
              (setq tab-width 2
                    c-basic-offset 2))))

;; C#
(use-package csharp-mode
  :ensure t
  :defer t
  :mode "\\.cs$"
  :config
  ;; Omnisharp (C# completion, refactoring, etc.)
  (use-package omnisharp
    :ensure t
    :defer t
    :mode "\\.cs$"
    :init
    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-to-list 'company-backends (sm/backend-with-yas
                                    'company-omnisharp)))
  :config
  (setq omnisharp-server-executable-path
        "~/Dev/omnisharp-server/OmniSharp.exe"))

;; dummy-h-mode
;; Determines c/c++/objc mode based on contents of a .h file.
(use-package dummy-h-mode
  :ensure t
  :mode "\\.h$")

;; aggressive-indent
;; Keeps code correctly indented during editing.
(use-package aggressive-indent
  :ensure t
  :commands (aggressive-indent-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'c-mode-common-hook 'aggressive-indent-mode))

;; undo-tree
;; Treat undo history as a tree.
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :mode ("\\.http$" . restclient-mode))

;; comment-dwim-2
;; Better `comment-dwim' supporting uncommenting.
(use-package comment-dwim-2
  :ensure t
  :commands (comment-dwim-2)
  :bind ("M-;" . comment-dwim-2))

;; circe
(use-package circe
  :ensure t
  :defer t
  :commands (circe)
  :config
  (enable-circe-color-nicks)
  (defun my-lui-setup ()
    (setq
     fringes-outside-margins t
     right-margin-width 7
     word-wrap t
     wrap-prefix "    "))
  (add-hook 'lui-mode-hook 'my-lui-setup)
  (setq lui-time-stamp-position 'right-margin
        lui-fill-type nil
        lui-scroll-behavior 'post-scroll))

;; column-enforce mode (highlight long columns).
(use-package column-enforce-mode
  :ensure t
  :diminish column-enforce-mode
  :commands (column-enforce-mode)
  :init
  (add-hook 'prog-mode-hook 'column-enforce-mode)
  (add-hook 'objc-mode-hook '(lambda ()
                               (setq-local column-enforce-column 100)))
  (add-hook 'java-mode-hook '(lambda ()
                               (setq-local column-enforce-column 100))))

;; Finally, if the compile-log window is active, kill it.
(let ((buf (get-buffer "*Compile-Log*")))
  (when buf (delete-windows-on buf)))

(provide 'init-packages)
