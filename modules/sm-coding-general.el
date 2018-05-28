;;; sm-coding-general.el --- General coding-related config.

;; EditorConfig.org -- project-local coding style definitions.
(use-package editorconfig
  :commands editorconfig-mode
  :delight editorconfig-mode
  :init (add-hook 'prog-mode-hook #'editorconfig-mode)
  :config
  (progn
    (add-to-list 'editorconfig-indentation-alist
                 '(swift-mode swift-indent-offset))))

;; highlight-numbers
;; Highlights magic numbers in programming modes.
(use-package highlight-numbers
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; rainbow-delimiters
;; Highlights parens, brackets, and braces according to their depth.
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; flycheck
(use-package flycheck
  :commands flycheck-mode
  :delight " ✓"
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config (setq flycheck-emacs-lisp-load-path 'inherit))

;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :commands restclient-mode
  :mode ("\\.http$" . restclient-mode))

;; lsp
;; Want to slowly replace ycmd with this where possible.
(use-package lsp-mode
  :config
  (setq lsp-enable-snippet t
        lsp-prefer-flymake nil))

(use-package lsp-ui
  :commands lsp-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t))

(use-package yaml-mode)

(provide 'sm-coding-general)
