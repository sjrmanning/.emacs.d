;; File: init-packages.el
;; Installs and configures third-party packages.

;; Required for running this code.
(require 'package)

;; Add the Melpa repository to the list of package sources.
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialise the package system.
(package-initialize)

;; Ensure use-package is installed.
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;; colors!
(use-package monokai-theme
  :ensure t
  :config
  (progn
    (setq monokai-distinct-fringe-background t)
    (load-theme 'monokai t)
    (set-face-background 'fringe 'unspecified)))

;; ag (silver surfer)
(use-package ag
  :ensure t
  :bind ("C-c s" . ag)
  :config (setq ag-highlight-search t))

;; anzu
(use-package anzu
  :ensure t
  :init (global-anzu-mode)
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; auto-complete
(use-package auto-complete
  :ensure t
  :defer t
  :diminish t
  :init
  (ac-config-default)
  (setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat")
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  (define-key ac-completing-map "\t" 'ac-complete))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config (setq mc/list-file "~/.emacs.d/etc/.mc-lists.el")
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;; expand-region
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; flycheck
(use-package flycheck-pos-tip
  :ensure t)
(use-package flycheck
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

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
  :defer t
  :bind ("C-x g" . magit-status))
(use-package monky
  :ensure t
  :defer t
  :bind ("C-c g" . monky-status))

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
(use-package ido-vertical-mode
  :ensure t)
(use-package ido-ubiquitous
  :ensure t)
(use-package flx-ido
  :ensure t)
(use-package ido
  :ensure t
  :init
  (progn
    (flx-ido-mode 1)
    (ido-everywhere 1)
    (ido-mode 1)
    (ido-vertical-mode 1)
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

;; ace-jump-mode
(use-package ace-jump-mode
  :ensure t
  :defer t
  :init
  (progn
    (autoload 'ace-jump-mode "ace-jump-mode" nil t)
    (bind-key "C-o" 'ace-jump-mode)))

;; markdown
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; smartparens
(use-package smartparens
  :ensure t
  :defer t
  :init
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)))

;; browse-kill-ring
(use-package browse-kill-ring
  :ensure t
  :defer t
  :bind ("M-y" . browse-kill-ring))

;; find file in repository.
(use-package find-file-in-repository
  :ensure t
  :bind ("C-x f" . find-file-in-repository))

;; smooth-scrolling
;; Avoids annoying behaviour when scrolling past the edges of a buffer.
(use-package smooth-scrolling
  :ensure t)

;; whitespace cleanup
;; Automatically cleans whitespace on save.
(use-package whitespace-cleanup-mode
  :ensure t
  :diminish t
  :init (global-whitespace-cleanup-mode))

;; uniquify.
;; Overrides Emacs' default mechanism for making buffer names unique.
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; Finally, if the compile-log window is active, kill it.
(let ((buf (get-buffer "*Compile-Log*")))
  (when buf (delete-windows-on buf)))

(provide 'init-packages)
