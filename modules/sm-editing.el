;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Require newline at end of file.
(setq require-final-newline t)

;; Revert buffers automatically when underlying files are changed externally.
(use-package autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode t))

;; Linum.
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format " %4d ")

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; General editing-related bindings.
(bind-key "C-w" 'backward-kill-word)
(bind-key "C-x C-k" 'kill-region)
(bind-key "C-c C-k" 'kill-region)

(bind-key "<f5>" 'sort-lines)
(bind-key "C-c b" 'switch-to-previous-buffer)

;; Cursor movement
(defun sm/next-line-fast ()
  "Faster `C-n'"
  (interactive)
  (ignore-errors (next-line 5)))

(defun sm/previous-line-fast ()
  "Faster `C-p'"
  (interactive)
  (ignore-errors (previous-line 5)))

(bind-key "C-S-n" 'sm/next-line-fast)
(bind-key "C-S-p" 'sm/previous-line-fast)

;; Crux (Collection of Ridiculously Useful eXtensions)
;; Replaces a lot of my old defuns and bindings.
(use-package crux
  :bind (("C-x C-r" . crux-recentf-ido-find-file)
         ("C-a" . crux-move-beginning-of-line)
         ("<S-return>" . crux-smart-open-line)
         ("C-c R" . crux-rename-buffer-and-file)
         ("C-c D" . crux-delete-buffer-and-file)
         ("<f2>" . crux-visit-term-buffer)
         ("s-j" . crux-top-join-line))
  :config (recentf-mode t))

;; Use conf-mode where appropriate.
(use-package conf-mode
  :mode (("\\.editorconfig$" . conf-mode)
         ("\\.conf" . conf-mode)
         ("\\.cfg" . conf-mode)
         ("\\.ini" . conf-mode)))

;; multiple-cursors
(use-package multiple-cursors
  :init (setq mc/list-file (sm/emacs.d "etc/.mc-lists.el"))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; smartparens
(use-package smartparens
  :defer 2
  :diminish " ()"
  :config
  (require 'smartparens-config)
  (sp-local-pair 'swift-mode "\\(" nil :actions nil)
  (sp-local-pair 'swift-mode "\\(" ")")
  (sp-local-pair 'swift-mode "<" ">")
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
  :bind ("M-y" . browse-kill-ring))

;; whitespace cleanup
;; Automatically cleans whitespace on save.
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :commands whitespace-cleanup-mode
  :init
  (add-hook 'text-mode-hook 'whitespace-cleanup-mode)
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

;; subword
(use-package subword
  :diminish subword-mode
  :init (global-subword-mode))

;; undo-tree
;; Treat undo history as a tree.
(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

;; smart-comment
;; Better `comment-dwim' supporting uncommenting.
(use-package smart-comment
  :bind ("M-;" . smart-comment))

;; embrace
;; Add/Change/Delete pairs based on expand-region.
(use-package embrace
  :bind ("C-," . embrace-commander))

;; aggressive-indent
;; Keeps code correctly indented during editing.
(use-package aggressive-indent
  :commands aggressive-indent-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode))

(provide 'sm-editing)
