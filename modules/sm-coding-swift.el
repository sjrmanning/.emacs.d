;;; sm-coding-swift.el --- Swift configuration.

;; swift
(use-package swift-mode
  :commands swift-mode)

(use-package flycheck-swift
  :hook (swift-mode . flycheck-swift-setup))

(provide 'sm-coding-swift)
