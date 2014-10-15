;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Require newline at end of file.
(setq require-final-newline t)

;; Linum.
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format " %4d ")

;; Don't use tabs for indent; replace tabs with four spaces.
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(provide 'init-editing)
