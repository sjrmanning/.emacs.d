;;; sm-coding-swift.el --- Swift configuration.

;; swift
(use-package swift-mode
  :commands swift-mode
  :hook (swift-mode . lsp)
  :mode ("\\.swift\\'" . swift-mode))

(use-package flycheck-swift
  :hook (swift-mode . flycheck-swift-setup)
  :config
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk"
        flycheck-swift-target "arm64-apple-ios13.0"))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-5.1.4-RELEASE.xctoolchain")
  (setq lsp-sourcekit-executable "/Library/Developer/Toolchains/swift-5.1.4-RELEASE.xctoolchain/usr/bin/sourcekit-lsp"))

(provide 'sm-coding-swift)
