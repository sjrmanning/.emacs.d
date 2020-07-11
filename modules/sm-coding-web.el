;;; sm-coding-web.el --- web related coding configuration.

(use-package slim-mode
  :mode ("\\.slim\\'" . slim-mode))

(use-package sass-mode
  :mode ("\\.sass\\'" . sass-mode))

(use-package web-mode
  :mode (("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :custom ((web-mode-markup-indent-offset 2)
           (web-mode-css-indent-offset 2)
           (web-mode-code-indent-offset 2)))

(provide 'sm-coding-web)
