;;; sm-coding-swift.el --- Swift configuration.

;; swift
(use-package swift-mode
  :commands swift-mode
  :hook (swift-mode . lsp)
  :mode ("\\.swift\\'" . swift-mode)
  :config
  (use-package flycheck-swift
    :config
    (flycheck-swift-setup)))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "~/Projects/sourcekit-lsp/.build/debug/sourcekit-lsp")))

(provide 'sm-coding-swift)
