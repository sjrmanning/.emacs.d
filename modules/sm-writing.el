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
  ;; :hook (text-mode . olivetti-mode)
  :bind ("C-c o" . olivetti-mode)
  :config
  (setq olivetti-body-width 100))

;; flyspell spell checking.
(use-package flyspell
  :delight flyspell-mode
  :commands flyspell-mode
  :init
  (setq flyspell-use-meta-tab nil) ; bbdb uses this key-chord, hands off!
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq ispell-extra-args '("--sug-mode=fast"))
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil))

(use-package flyspell-correct-ivy
  :after flyspell
  :commands flyspell-correct-word-generic
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-word-generic))
  :custom (flyspell-correct-interface 'flyspell-correct-ivy))

;; lorem ipsum, generate fun text
(use-package lorem-ipsum)

;; markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (add-hook 'markdown-mode-hook (lambda () (setq display-line-numbers t)))
  )

(use-package markdownfmt
  :config
  (progn
    ;; (add-hook 'markdown-mode-hook #'markdownfmt-enable-on-save)
    ))

(provide 'sm-writing)
