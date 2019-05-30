;;; sm-coding-swift.el --- Swift configuration.

;; swift
(use-package swift-mode
  :commands swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :config
  (use-package flycheck-swift
    :config
    (flycheck-swift-setup)))

(use-package lsp-sourcekit
  :commands lsp
  :straight (lsp-sourcekit :type git :host github :repo "emacs-lsp/lsp-sourcekit")
  :init (add-hook 'swift-mode-hook #'lsp)
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "~/Projects/sourcekit-lsp/.build/debug/sourcekit-lsp")))

(provide 'sm-coding-swift)
