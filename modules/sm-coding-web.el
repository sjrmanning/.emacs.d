;;; sm-coding-web.el --- web related coding configuration.

(use-package php-mode
  :mode "\\.php[345]?\\'"
  :config
  (add-hook 'php-mode-hook #'ggtags-mode))

(use-package slim-mode
  :mode ("\\.slim\\'" . slim-mode))

(use-package sass-mode
  :mode ("\\.sass\\'" . sass-mode))

(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :config (progn
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)))

(provide 'sm-coding-web)
