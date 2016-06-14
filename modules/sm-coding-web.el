;;; sm-coding-web.el --- web related coding configuration.

(use-package php-mode
  :mode "\\.php[345]?\\'"
  :config
  (add-hook 'php-mode-hook #'ggtags-mode))

(provide 'sm-coding-web)
