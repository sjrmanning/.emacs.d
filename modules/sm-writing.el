;; typo
;; Mode for typographical editing.
(use-package typo
  :hook text-mode
  :custom (typo-language "English"))

;; olivetti -- similar to writeroom but a simple minor mode
(use-package olivetti
  :if window-system
  :delight
  :hook text-mode
  :bind ("C-c o" . olivetti-mode)
  :custom
  (olivetti-minimum-body-width 80)
  (olivetti-body-width 0.66))

(use-package flyspell
  :delight
  :hook (text-mode git-commit-mode (prog-mode . flyspell-prog-mode))
  :custom
  (ispell-extra-args '("--sug-mode=ultra" "--run-together"))
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (flyspell-prog-text-faces '(tree-sitter-hl-face:comment
                              tree-sitter-hl-face:doc
                              tree-sitter-hl-face:string
                              font-lock-comment-face
                              font-lock-doc-face
                              font-lock-string-face)))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom (markdown-command "pandoc"))

;; Double spaces at the end of sentences is a bit outdated.
(setq sentence-end-double-space nil)

(provide 'sm-writing)
