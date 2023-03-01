;;; sm-coding-python.el --- Python setup.

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config (eglot-ensure))

(provide 'sm-coding-python)
