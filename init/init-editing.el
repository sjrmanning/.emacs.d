;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Require newline at end of file.
(setq require-final-newline t)

;; Linum.
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format " %4d ")

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Prompt to automatically create parent dirs when required.
(add-to-list 'find-file-not-found-functions #'sm/create-non-existent-directory)

(provide 'init-editing)
