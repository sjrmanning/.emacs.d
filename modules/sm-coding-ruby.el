;;; sm-coding-ruby.el --- Ruby configuration.

(use-package ruby-mode
  :mode (("Fastfile$" . ruby-mode)
         ("Appfile$" . ruby-mode)
         ("Podfile$" . ruby-mode)))

(provide 'sm-coding-ruby)
