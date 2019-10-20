;;; sm-coding-python.el --- Python setup.

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (use-package lsp-python-ms
    :init (require 'lsp-python-ms)
    :hook (python-mode . lsp)))

(provide 'sm-coding-python)
