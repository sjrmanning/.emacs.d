;; File: init-packages.el
;; Installs and configures packages, including third-party packages
;; which will be downloaded when required via `use-package'.

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

;; exec-path-from-shell
;; Use $PATH from user's shell in Emacs.
(use-package exec-path-from-shell
  :ensure t
  :commands (exec-path-from-shell-initialize))

;; yasnippet
(use-package yasnippet
  :ensure t
  :idle (yas-global-mode t)
  :init
  (progn
    ;; Suppress excessive log messages
    (setq yas-verbosity 1)

    ;; Ensure custom snippets dir exists.
    (defvar custom-snippets-dir "~/.emacs.d/etc/snippets/")
    (unless (file-exists-p custom-snippets-dir)
      (make-directory custom-snippets-dir))

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
  (progn
    (load-theme 'monokai t)))

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
  :diminish auto-complete-mode
  :config
  (ac-config-default)
  (setq ac-comphist-file "~/.emacs.d/cache/ac-comphist.dat"
        ac-delay 0.125
        ac-auto-show-menu 0.25
        ac-use-fuzzy t
        ac-use-quick-help t
        ac-quick-help-delay 1.0
        ac-use-menu-map t
        ac-ignore-case t)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
  (define-key ac-menu-map "\t" 'ac-complete))

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
  :diminish " ✓"
  :config
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
  :commands magit-status
  :bind ("C-x g" . magit-status))
(use-package monky
  :ensure t
  :commands monky-status
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
(use-package ido
  :ensure t
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-enable-flex-matching t
          ido-enable-prefix nil
          ido-max-prospects 10)))

(use-package ido-vertical-mode
  :ensure t
  :init (ido-vertical-mode 1))

(use-package ido-ubiquitous
  :ensure t
  :init (ido-everywhere 1))

(use-package flx-ido
  :ensure t
  :init (flx-ido-mode 1)
  :config
  (setq ido-use-faces nil))

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
  :defer t
  :bind ("C-x f" . find-file-in-repository))

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
  :commands dtrt-indent-mode
  :init
  (add-hook 'prog-mode-hook 'dtrt-indent-mode))

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

;; subword
(use-package subword
  :diminish subword-mode
  :init (global-subword-mode))

;; python
;; Configures jedi to run with python-mode.
(use-package python
  :commands python-mode
  :config
  (progn
    (use-package jedi
      :ensure t
      :config
      (progn
        (jedi:setup)
        (jedi:ac-setup)
        (setq jedi:setup-keys t)
        (setq jedi:complete-on-dot t)))
    (add-hook 'python-mode-hook (lambda () (jedi-mode t)))))

;; cc-mode/derived modes and hooks
(use-package cc-mode
  :defer t
  :config
  (progn
    (add-hook 'java-mode-hook
              (lambda ()
                (setq tab-width 2
                      c-basic-offset 2)))))

;; twittering-mode
(use-package twittering-mode
  :defer t
  :ensure t
  :config
  (progn
    (setq twittering-icon-mode t
          twittering-use-master-password t)))

;; Finally, if the compile-log window is active, kill it.
(let ((buf (get-buffer "*Compile-Log*")))
  (when buf (delete-windows-on buf)))

(provide 'init-packages)
