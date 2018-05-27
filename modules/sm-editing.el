;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Require newline at end of file.
(setq require-final-newline t)

;; map pairs of simultaneously/rapidly pressed keys to commands
;; other packages use key-chord-define*, so hook it up early
(use-package key-chord
  :init
  (progn
    (key-chord-define-global "xx" 'execute-extended-command)
    (key-chord-mode +1)))

;; Revert buffers automatically when underlying files are changed externally.
(use-package autorevert
  :hook (after-init . global-auto-revert-mode)
  :delight auto-revert-mode)

;; Native line numbers won't work until 26.x.  In the meantime, use linum
;; (add-hook 'prog-mode-hook (lambda () (setq display-line-numbers t)))
(add-hook 'prog-mode-hook
          (lambda ()
            (linum-mode +1)
            (setq linum-format " %4d ")))

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; george is weird.  swap backspace and del at very low level
(keyboard-translate ?\C-h ?\C-?)

;; General editing-related bindings.
(bind-key "\e \C-g" 'goto-line)
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
  :bind (("C-x C-r" . crux-recentf-find-file)
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
  :hook
  (after-init .
              (lambda ()
                (require 'multiple-cursors)
                (set-face-attribute
                 'mc/cursor-bar-face nil
                 :background (face-attribute 'cursor :background)
                 :foreground (face-attribute 'cursor :background)
                 :height 0.2)
                (setq mc/list-file (sm/emacs.d "etc/.mc-lists.el"))))
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; smartparens
(use-package smartparens
  :hook
  (after-init . (lambda ()
                  (smartparens-global-mode t)
                  (show-smartparens-global-mode t)))
  :delight " ()"
  :bind
  ((:map sp-keymap)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)))

;; browse-kill-ring
(use-package browse-kill-ring
  :config
  (key-chord-define-global "yy" 'browse-kill-ring))

;; whitespace
(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing lines-tail))
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  )
;; whitespace cleanup
;; Automatically cleans whitespace on save.
(use-package whitespace-cleanup-mode
  :delight whitespace-cleanup-mode
  :commands whitespace-cleanup-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-cleanup-mode)))

;; subword
(use-package subword
  :hook (after-init . global-subword-mode)
  :delight subword-mode)

;; undo-tree
;; Treat undo history as a tree.
(use-package undo-tree
  :straight (undo-tree :type git :host github :repo "martinp26/undo-tree")
  :delight undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (unbind-key "C-x u" undo-tree-map)
  (key-chord-define-global "uu" 'undo-tree-visualize))

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
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode))

;; encryption, the way that I'm used to doing it.
(use-package crypt++
  :straight nil
  :load-path "etc/extra"
  :config
  (progn
    (epa-file-disable)
    (setq crypt-encryption-type 'gpg)
    (setq crypt-encryption-file-extension "\\(\\.gpg\\)$")
    (modify-coding-system-alist 'file "\\.bz\\'" 'no-conversion)
    (modify-coding-system-alist 'file "\\.bz2\\'" 'no-conversion)
    (modify-coding-system-alist 'file "\\.gpg\\'" 'no-conversion)
    (modify-coding-system-alist 'file "\\.gz\\'" 'no-conversion)
    (modify-coding-system-alist 'file "\\.Z\\'" 'no-conversion)
    ))

(provide 'sm-editing)
