;;; sm-coding-swift.el --- Swift configuration.

;; swift
(use-package swift-mode
  :commands swift-mode)

(use-package flycheck-swift
  :hook (swift-mode . flycheck-swift-setup)
  :custom
  (flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk")
  (flycheck-swift-target "arm64-apple-ios13.4"))

(use-package lsp-sourcekit
  :hook (swift-mode . lsp)
  :hook (swift-mode . (lambda () (require 'lsp-sourcekit)))
  :config
  (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(provide 'sm-coding-swift)
