;;; sm-coding-python.el --- Python and anaconda-mode setup.

(use-package python
  :commands python-mode
  :init (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (use-package anaconda-mode
    :commands anaconda-mode)
  (use-package company-anaconda
    :defer t
    :init
    (add-to-list 'company-backends
                 (sm/backend-with-yas 'company-anaconda))
    :config
    (setq anaconda-mode-installation-directory
          (sm/cache-for "anaconda-mode"))))

(provide 'sm-coding-python)
