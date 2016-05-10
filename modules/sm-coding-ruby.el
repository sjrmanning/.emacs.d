;;; sm-coding-ruby.el --- Ruby configuration.

(use-package ruby-mode
  :ensure ruby-tools
  :ensure inf-ruby
  :interpreter "ruby"
  :mode (("Fastfile$" . ruby-mode)
         ("Appfile$" . ruby-mode))
  :config
  (add-hook 'ruby-mode-hook
            (lambda ()
              (inf-ruby-minor-mode t)
              (ruby-tools-mode t))))

(provide 'sm-coding-ruby)
