;;; sm-coding-general.el --- General coding-related config.

;; EditorConfig.org -- project-local coding style definitions.
(use-package editorconfig
  :delight
  :hook prog-mode
  :config
  (add-to-list 'editorconfig-indentation-alist
               '(swift-mode swift-indent-offset)))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" "Procfile\\'")
  :bind (:map yaml-mode-map ("C-m" . newline-and-indent)))

;; highlight-numbers
;; Highlights magic numbers in programming modes.
(use-package highlight-numbers
  :hook prog-mode)

;; rainbow-delimiters
;; Highlights parens, brackets, and braces according to their depth.
(use-package rainbow-delimiters
  :hook prog-mode)

;; flycheck
(use-package flycheck
  :delight " ✓"
  :hook prog-mode
  :custom (flycheck-emacs-lisp-load-path 'inherit))

;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :mode ("\\.http$" . restclient-mode))

;; eglot
(use-package eglot
  :commands (eglot-ensure eglot)
  :straight (:type built-in))

;; protobuf
(use-package protobuf-mode
  :mode (("\\.proto$" . protobuf-mode)
         ("\\.proto3$" . protobuf-mode)))

;; Lua
(use-package lua-mode
  :straight (:build (:not autoloads))
  :commands lua-mode
  :mode (("\\.lua$" . lua-mode)))

;; topsy
;; Sticky header showing parent definitions of top line.
(use-package topsy
  :hook prog-mode)

(provide 'sm-coding-general)
