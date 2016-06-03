;;; sm-coding-python.el --- Python and anaconda-mode setup.

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(provide 'sm-coding-python)
