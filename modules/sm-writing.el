;; text-mode  -*- lexical-binding: t; -*-
;; Disable ispell showing word completions in corfu.
(use-package text-mode
  :ensure nil
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              (remove-hook 'completion-at-point-functions
                           'ispell-completion-at-point t))))

;; typo
;; Mode for typographical editing.
(use-package typo
  :ensure (:type git :host github :repo "jorgenschaefer/typoel")
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
  :hook (text-mode git-commit-mode) ;; swift-mode is lagging with jinx.
  :bind (([remap ispell-word] . jinx-correct)
         ("C-;" . jinx-correct)))

;; markdown
;; (use-package markdown-ts-mode
;;   :ensure nil
;;   :mode (("\\.md\\'" . markdown-ts-mode)
;;          ;; ("README\\.md\\'" . gfm-mode)
;;          ("\\.markdown\\'" . markdown-ts-mode)))
  ;; :custom
  ;; (markdown-command "pandoc")
  ;; (markdown-fontify-code-blocks-natively t)
  ;; )

;; markdown live-preview with xwidgets when available.
;; (when (and (display-graphic-p) (featurep 'xwidget-internal))
;; (use-package markdown-xwidget
;;   :after markdown-mode
;;   :commands (markdown-xwidget-preview-mode markdown-xwidget-preview)
;;   :ensure (markdown-xwidget
;;              :type git
;;              :host github
;;              :repo "cfclrk/markdown-xwidget"
;;              :files (:defaults "resources"))
;;   :custom
;;   (markdown-xwidget-github-theme "light")
;;   :init
;;   (bind-keys :map markdown-mode-map
;;              ("C-c C-c l" . markdown-xwidget-preview-mode))))

;; Double spaces at the end of sentences is a bit outdated.
(setq sentence-end-double-space nil)

(provide 'sm-writing)
