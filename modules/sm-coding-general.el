;;; sm-coding-general.el --- General coding-related config.  -*- lexical-binding: t; -*-

;; Native line numbers, enabled for any programming mode.
(use-package display-line-numbers
  :ensure nil
  :hook prog-mode)

;; treesit (using built-in)
(use-package treesit
  :ensure nil
  :custom
  (treesit--install-language-grammar-out-dir-history (list (sm/emacs.d "etc/treesit-modules")))
  (treesit-extra-load-path (list (sm/emacs.d "etc/treesit-modules"))))

(use-package emacs
  :ensure nil
  :config
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (js-json-mode . json-ts-mode)
          (python-mode . python-ts-mode))))

;; EditorConfig.org -- project-local coding style definitions.
;; (use-package editorconfig
;;   :diminish
;;   :hook prog-mode)

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
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :mode ("\\.http$" . restclient-mode))

;; eglot
(use-package eglot-booster
  :ensure (:type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config	(eglot-booster-mode))

(use-package eglot
  :ensure nil
  :commands (eglot-ensure eglot)
  ;; :straight (:type built-in)
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
  (add-to-list 'eglot-server-programs '((swift-mode) "xcrun" "sourcekit-lsp"))
  (add-to-list 'eglot-server-programs '((objc-mode) "clangd")))

(use-package eldoc-box
  :hook ((eglot-managed-mode emacs-lisp-mode) . eldoc-box-hover-mode))

(use-package consult-eglot
  :after eglot
  :commands (consult-eglot-symbols))

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
