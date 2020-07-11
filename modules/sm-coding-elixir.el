;;; sm-coding-elixir.el --- Elixir configuration.

(use-package elixir-mode
  :interpreter "elixir"
  :hook (elixir-mode . lsp)
  :custom (lsp-clients-elixir-server-executable
           "/usr/local/bin/elixir-ls/language_server.sh"))

(provide 'sm-coding-elixir)
