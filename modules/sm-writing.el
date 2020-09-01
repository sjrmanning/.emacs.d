;; typo
;; Mode for typographical editing.
(use-package typo
  :commands typo-mode
  :config (setq-default typo-language "English")
  :init (add-hook 'text-mode-hook #'typo-mode))

;; Distraction-free writing.
(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (add-to-list 'writeroom-global-effects 'visual-line-mode)
  (setq writeroom-restore-window-config t
        writeroom-width 100))

;; olivetti -- similar to writeroom but a simple minor mode
(use-package olivetti
  :if window-system
  :delight
  :hook (text-mode . olivetti-mode)
  :bind ("C-c o" . olivetti-mode)
  :custom
  (olivetti-minimum-body-width 80)
  (olivetti-body-width 0.66))

;; flyspell spell checking.
(use-package flyspell
  :delight flyspell-mode
  :commands flyspell-mode
  :hook (text-mode . flyspell-mode)
  :config
  (setq ispell-extra-args '("--sug-mode=fast"))
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc"))

;; Double spaces at the end of sentences is a bit outdated.
(setq sentence-end-double-space nil)

(provide 'sm-writing)
