;; typo
;; Mode for typographical editing.
(use-package typo
  :commands typo-mode
  :config (setq-default typo-language "English")
  :init (add-hook 'text-mode-hook #'typo-mode))

;; flyspell spell checking.
(use-package flyspell
  :diminish flyspell-mode
  :commands flyspell-mode
  :init (add-hook 'text-mode-hook #'flyspell-mode)
  :config
  (setq ispell-extra-args '("--sug-mode=fast"))
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil))

;; markdown
(use-package markdown-mode
  :mode "\\.md\\'")

;; Double spaces at the end of sentences is a bit outdated.
(setq sentence-end-double-space nil)

(provide 'sm-writing)
