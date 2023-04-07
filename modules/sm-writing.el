;; typo
;; Mode for typographical editing.
(use-package typo
  :hook text-mode)

;; olivetti -- similar to writeroom but a simple minor mode
(use-package olivetti
  :if (display-graphic-p)
  :diminish
  :hook text-mode
  :bind ("C-c o" . olivetti-mode)
  :custom
  (olivetti-minimum-body-width 80)
  (olivetti-body-width 0.66))

(use-package jinx
  :hook (text-mode git-commit-mode prog-mode)
  :bind (([remap ispell-word] . jinx-correct)
         ("C-;" . jinx-correct)))

;; markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t))

;; markdown live-preview with xwidgets when available.
(use-package markdown-xwidget
  :if (and (display-graphic-p) (featurep 'xwidget-internal))
  :after markdown-mode
  :commands (markdown-xwidget-preview-mode markdown-xwidget-preview)
  :straight (markdown-xwidget
             :type git
             :host github
             :repo "cfclrk/markdown-xwidget"
             :files (:defaults "resources"))
  :custom
  (markdown-xwidget-github-theme "light")
  :init
  (bind-keys :map markdown-mode-map
             ("C-c C-c l" . markdown-xwidget-preview-mode)))

;; Double spaces at the end of sentences is a bit outdated.
(setq sentence-end-double-space nil)

(provide 'sm-writing)
