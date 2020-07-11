;;; sm-coding-java.el --- Java and Android config.

(use-package kotlin-mode
  :hook (kotlin-mode . lsp)
  :config
  (setq kotlin-tab-width 4))

(provide 'sm-coding-java)
