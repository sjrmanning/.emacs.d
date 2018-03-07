;;; sm-coding-elixir.el --- Elixir configuration.

(use-package alchemist
  :commands (elixir-mode alchemist-mode)
  :delight alchemist-mode)

(use-package elixir-mode
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode))
  :config
  (alchemist-mode t))

(provide 'sm-coding-elixir)
