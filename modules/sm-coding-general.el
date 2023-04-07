;;; sm-coding-general.el --- General coding-related config.

;; Native line numbers, enabled for any programming mode.
(use-package display-line-numbers
  :straight (:type built-in)
  :hook prog-mode)

;; treesit (using built-in)
(use-package treesit
  :straight (:type built-in)
  :mode (("Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.rs\\'" . rust-ts-mode))
  :custom
  (treesit-extra-load-path (list (sm/emacs.d "etc/treesit-modules")))
  :init
  ;; Replace built-in modes with ts equivalent.
  (dolist (mode
           '((bash-mode       . bash-ts-mode)
             (c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (c-or-c++-mode   . c-or-c++-ts-mode)
             (css-mode        . css-ts-mode)
             (dockerfile-mode . dockerfile-ts-mode)
             (go-mode         . go-ts-mode)
             (java-mode       . java-ts-mode)
             (javascript-mode . js-ts-mode)
             (js-mode         . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (python-mode     . python-ts-mode)
             (ruby-mode       . ruby-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (yaml-mode       . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mode)))

;; EditorConfig.org -- project-local coding style definitions.
(use-package editorconfig
  :diminish
  :hook prog-mode)

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
  :diminish " ✓"
  :hook prog-mode
  :custom (flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :mode ("\\.http$" . restclient-mode))

;; eglot
(use-package eglot
  :commands (eglot-ensure eglot)
  :straight (:type built-in)
  :hook ((swift-mode
          python-mode
          python-ts-mode
          rust-ts-mode
          c-mode
          c-ts-mode
          c++-mode
          c++-ts-mode
          objc-mode) . eglot-ensure)
  :custom
  (eglot-sync-connect 0)
  :config
  (add-to-list 'eglot-server-programs '((swift-mode) "sourcekit-lsp"))
  (add-to-list 'eglot-server-programs '((objc-mode) "clangd")))

;; protobuf
(use-package protobuf-mode
  :mode (("\\.proto$" . protobuf-mode)
         ("\\.proto3$" . protobuf-mode)))

;; Lua
(use-package lua-mode
  :commands lua-mode
  :mode (("\\.lua$" . lua-mode)))

;; topsy
;; Sticky header showing parent definitions of top line.
(use-package topsy
  :hook prog-mode)

(provide 'sm-coding-general)
