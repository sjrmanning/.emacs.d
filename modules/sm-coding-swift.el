;;; sm-coding-swift.el --- Swift configuration.

;; swift
(use-package swift-mode
  :commands swift-mode
  :hook (swift-mode . lsp)
  :mode ("\\.swift\\'" . swift-mode))

(use-package flycheck-swift
  :hook (swift-mode . flycheck-swift-setup)
  :config
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS12.2.sdk"
        flycheck-swift-target "arm64-apple-ios12.2"))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
  (setq lsp-sourcekit-executable (expand-file-name "~/Projects/sourcekit-lsp/.build/debug/sourcekit-lsp")))

(provide 'sm-coding-swift)
