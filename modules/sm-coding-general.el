;;; sm-coding-general.el --- General coding-related config.

;; EditorConfig.org -- project-local coding style definitions.
(use-package editorconfig
  :delight editorconfig-mode
  :hook (prog-mode . editorconfig-mode)
  :config
    (add-to-list 'editorconfig-indentation-alist
                 '(swift-mode swift-indent-offset)))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" "Procfile\\'")
  :bind (:map yaml-mode-map ("C-m" . newline-and-indent)))

;; highlight-numbers
;; Highlights magic numbers in programming modes.
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; rainbow-delimiters
;; Highlights parens, brackets, and braces according to their depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; flycheck
(use-package flycheck
  :delight " ✓"
  :hook (prog-mode . flycheck-mode)
  :custom (flycheck-emacs-lisp-load-path 'inherit))

;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :mode ("\\.http$" . restclient-mode))

;; lsp
(use-package lsp-mode
  :custom (lsp-enable-snippet t))

(use-package lsp-ui
  :after lsp-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-ignore-duplicate t))

;; protobuf
(use-package protobuf-mode
  :mode (("\\.proto$" . protobuf-mode)
         ("\\.proto3$" . protobuf-mode)))

;; tree-sitter
(use-package tree-sitter-langs :ensure t)
(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(provide 'sm-coding-general)
