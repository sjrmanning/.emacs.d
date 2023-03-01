;;; sm-coding-swift.el --- Swift configuration.

;; swift
(use-package swift-mode
  :commands swift-mode
  :config
  (eglot-ensure)
  (add-to-list 'eglot-server-programs
    '(swift-mode . ("xcrun" "sourcekit-lsp"))))

(use-package flycheck-swift
  :hook (swift-mode . flycheck-swift-setup))

(provide 'sm-coding-swift)
