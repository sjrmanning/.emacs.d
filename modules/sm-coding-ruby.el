;;; sm-coding-ruby.el --- Ruby configuration.

(use-package ruby-mode
  :straight ruby-tools
  :straight inf-ruby
  :interpreter "ruby"
  :mode (("Fastfile$" . ruby-mode)
         ("Appfile$" . ruby-mode)
         ("Podfile$" . ruby-mode))
  :hook (ruby-mode . (lambda ()
                       (inf-ruby-minor-mode t)
                       (ruby-tools-mode t))))

(provide 'sm-coding-ruby)
