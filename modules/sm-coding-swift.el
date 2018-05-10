;;; sm-coding-swift.el --- Swift configuration.

;; swift
(use-package swift-mode
  :commands swift-mode
  :mode ("\\.swift\\'" . swift-mode)
  :config
  (use-package flycheck-swift
    :config
    (flycheck-swift-setup))
  ;; (ycmd-mode -1)
  ;; (use-package company-sourcekit
  ;;   :init (setq company-sourcekit-use-yasnippet t)
  ;;   :config
  ;;   (setq sourcekit-verbose t)
  ;;   (add-to-list 'company-backends 'company-sourcekit))
  )

(provide 'sm-coding-swift)
