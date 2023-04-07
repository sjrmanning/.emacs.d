;;; sm-editing.el --- General editing improvements and defaults.  -*- lexical-binding: t; -*-
(add-hook 'after-init-hook
          (lambda nil
            ;; Delete marked text on typing
            (delete-selection-mode t)
            ;; Soft-wrap lines
            (global-visual-line-mode t)
            (diminish 'visual-line-mode)
            ;; Auto revert buffers when files change.
            (global-auto-revert-mode t)
            ;; Native pair mode.
            (electric-pair-mode t)))

;; Require newline at end of file.
(setq require-final-newline t)

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; General editing-related bindings.
;; Cursor movement
(defun sm/next-line-fast ()
  "Faster `C-n'"
  (interactive)
  (ignore-errors (next-line 5)))

(defun sm/previous-line-fast ()
  "Faster `C-p'"
  (interactive)
  (ignore-errors (previous-line 5)))

(bind-key "C-w" #'backward-kill-word)
(bind-key "C-x C-k" #'kill-region)
(bind-key "C-S-n" #'sm/next-line-fast)
(bind-key "C-S-p" #'sm/previous-line-fast)
(bind-key "<f5>" #'sort-lines)

;; Crux (Collection of Ridiculously Useful eXtensions)
;; Replaces a lot of my old defuns and bindings.
(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)
         ("<S-return>" . crux-smart-open-line)
         ("C-c R" . crux-rename-buffer-and-file)
         ("C-c D" . crux-delete-buffer-and-file)
         ("s-j" . crux-top-join-line)))

;; Use conf-mode where appropriate.
(use-package conf-mode
  :mode (("\\.editorconfig$" . conf-mode)
         ("\\.conf" . conf-mode)
         ("\\.cfg" . conf-mode)
         ("\\.ini" . conf-mode)))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this))
  :custom (mc/list-file (sm/emacs.d "etc/.mc-lists.el"))
  :custom-face
  (mc/cursor-bar-face
   ((t (:height 0.2 :background "#657b83"
                :foreground "#657b83")))))

;; expand-region
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; whitespace cleanup
;; Automatically cleans whitespace on save.
(use-package whitespace-cleanup-mode
  :diminish
  :hook (text-mode prog-mode))

;; subword
(use-package subword
  :diminish
  :hook (after-init . global-subword-mode))

;; smart-comment
;; Better `comment-dwim' supporting uncommenting.
(use-package smart-comment
  :bind ("M-;" . smart-comment))

;; embrace
;; Add/Change/Delete pairs based on expand-region.
(use-package embrace
  :bind ("C-," . embrace-commander))

(provide 'sm-editing)
