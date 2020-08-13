;;; sm-coding-python.el --- Python setup.

(defun py-isort-enable-on-save ()
  "Hook isort up to before save hook."
  (interactive)
  (add-hook 'before-save-hook 'py-isort-before-save nil t))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (use-package lsp-python-ms
    :init (require 'lsp-python-ms))
  (use-package py-isort)
  (use-package python-black)
  :hook (
         (python-mode . lsp-deferred)
         (python-mode . py-isort-enable-on-save)
         (python-mode . python-black-on-save-mode)
         )
  )

(setenv "PYTHONIOENCODING" "utf8")

(provide 'sm-coding-python)
