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

(require 'use-package)

;; exec-path-from-shell
;; Use $PATH from user's shell in Emacs.
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;; yasnippet
(use-package yasnippet
  :ensure t
  :idle (yas-global-mode t)
  :init
  (progn
    ;; Suppress excessive log messages
    (setq yas-verbosity 1)

    ;; Ensure custom snippets dir exists.
    (defvar custom-snippets-dir (sm/emacs.d "etc/snippets/"))
    (sm/mkdir-p custom-snippets-dir)

    ;; Replace default custom dir with our own.
    (setq yas-snippet-dirs '(custom-snippets-dir
                             yas-installed-snippets-dir))

    ;; Disable yasnippet in some modes.
    (defun yas-disable-hook ()
      (setq yas-dont-activate t))
    (add-hook 'term-mode-hook 'yas-disable-hook)
    (add-hook 'erc-mode-hook 'yas-disable-hook)))

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

;; auto-complete
(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (progn
    ;; Source for `completion-at-point'.
    (use-package ac-capf
      :ensure t
      :commands (ac-capf-setup)
      :init
      (progn
        (add-hook 'inferior-python-mode-hook 'ac-capf-setup)
        (add-hook 'comint-mode-hook 'ac-capf-setup)))

    ;; Standard auto-complete settings.
    (ac-config-default)
    (setq ac-comphist-file (sm/emacs.d "cache/ac-comphist.dat")
          ac-delay 0.125
          ac-auto-show-menu 0.25
          ac-use-fuzzy t
          ac-use-quick-help t
          ac-quick-help-delay 1.0
          ac-use-menu-map t
          ac-ignore-case t)
    (setq-default ac-sources '(ac-source-filename
                               ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers))
    (define-key ac-menu-map (kbd "C-n") 'ac-next)
    (define-key ac-menu-map (kbd "C-p") 'ac-previous)
    (define-key ac-menu-map "\t" 'ac-complete)))

;; org-mode
(use-package org
  :ensure t
  :commands (org-mode)
  :config
  (progn
    (setq org-src-fontify-natively t)))

;; deft
(use-package deft
  :ensure t
  :commands (deft)
  :bind ("M-<f1>" . deft)
  :config
  (progn
    (setq
     deft-extension "org"
     deft-directory "~/Org/deft/"
     deft-text-mode 'org-mode
     deft-use-filename-as-title t
     deft-auto-save-interval 30.0)))

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
  :init
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode)))

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
  :config
  (progn
    (add-hook 'git-commit-mode-hook '(lambda () (flyspell-mode t)))))
(use-package monky
  :ensure t
  :commands (monky-status)
  :bind ("C-c g" . monky-status)
  :config
  (progn
    (add-hook 'monky-log-edit-mode-hook '(lambda () (flyspell-mode t)))))

;; git-gutter
(use-package git-gutter-fringe
  :ensure t)
(use-package git-gutter
  :ensure t
  :init
  (progn
    (require 'git-gutter-fringe)
    (global-git-gutter-mode t)))
  
;; smex
(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :init
  (smex-initialize))

;; ido
(use-package ido
  :ensure t
  :init (ido-mode 1)
  :config
  (progn
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
    (flx-ido-mode 1)))

;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :commands (ace-jump-mode)
  :bind ("C-o" . ace-jump-mode))

;; markdown
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; smartparens
(use-package smartparens
  :ensure t
  :defer t
  :diminish " ()"
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)

    ;; sp keybindings.
    (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
    (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
    (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
    (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

    (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
    (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)))

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
  (progn
    ;; Ensure projectile dir exists.
    (defvar my-projectile-dir (sm/emacs.d "cache/projectile"))
    (sm/mkdir-p my-projectile-dir)

    ;; Use projectile dir for cache and bookmarks.
    (let* ((prj-dir (file-name-as-directory my-projectile-dir))
           (prj-cache-file (concat prj-dir "projectile.cache"))
           (prj-bookmarks-file (concat prj-dir "projectile-bkmrks.eld")))
      (setq projectile-cache-file          prj-cache-file
            projectile-known-projects-file prj-bookmarks-file
            projectile-indexing-method     'alien))))

;; saveplace
;; Remebers your location in a file when saving files.
(use-package saveplace
  :init
  (progn
    (setq save-place-file (sm/emacs.d "cache/saveplace"))
    (setq-default save-place t)))

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

;; dtrt-indent
;; Determines what indentation offset to use on files.
(use-package dtrt-indent
  :ensure t
  :diminish dtrt-indent-mode
  :commands (dtrt-indent-mode)
  :init (add-hook 'prog-mode-hook 'dtrt-indent-mode))

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
  (progn
    (add-hook 'prog-mode-hook 'highlight-numbers-mode)))

;; rainbow-delimiters
;; Highlights parens, brackets, and braces according to their depth.
(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)))

;; python
;; Configures jedi to run with python-mode.
(use-package python
  :commands (python-mode)
  :config
  (progn
    (use-package jedi
      :ensure t
      :commands (jedi:setup)))
  :init
  (progn
    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:complete-on-dot t)))

;; cc-mode/derived modes and hooks
(use-package cc-mode
  :defer t
  :config
  (progn
    (add-hook 'objc-mode-hook
              (lambda ()
                (setq c-basic-offset 4)))
    (add-hook 'java-mode-hook
              (lambda ()
                (c-set-offset 'arglist-intro '+)
                (c-set-offset 'inexpr-class 0)
                (setq tab-width 2
                      c-basic-offset 2)))))

;; dummy-h-mode
;; Determines c/c++/objc mode based on contents of a .h file.
(use-package dummy-h-mode
  :ensure t
  :mode ("\\.h$" . dummy-h-mode))

;; aggressive-indent
;; Keeps code correctly indented during editing.
(use-package aggressive-indent
  :ensure t
  :commands (aggressive-indent-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook 'aggressive-indent-mode)))

;; undo-tree
;; Treat undo history as a tree.
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :mode ("\\.http$" . restclient-mode))

;; Finally, if the compile-log window is active, kill it.
(let ((buf (get-buffer "*Compile-Log*")))
  (when buf (delete-windows-on buf)))

(provide 'init-packages)
