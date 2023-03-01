;;; sm-coding-java.el --- Java and Android config.

(use-package kotlin-mode
  :config
  (eglot-ensure)
  (setq kotlin-tab-width 4))

(provide 'sm-coding-java)
