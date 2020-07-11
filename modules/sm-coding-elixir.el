;;; sm-coding-elixir.el --- Elixir configuration.

(use-package alchemist
  :after elixir-mode
  :hook (elixir-mode . alchemist-mode)
  :hook (elixir-mode . flycheck-mode))

(use-package elixir-mode
  :interpreter "elixir")

(provide 'sm-coding-elixir)
