;;; sm-coding-python.el --- Python setup.

(defun py-isort-enable-on-save ()
  "Hook isort up to before save hook."
  (interactive)
  (add-hook 'before-save-hook 'py-isort-before-save nil t))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
              ("M-*" . pop-tag-mark))
  :interpreter ("python" . python-mode))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package py-isort
  :demand t
  :after python
  :hook (python-mode . py-isort-enable-on-save))

;; (use-package lsp-pyright
;;   :straight `(lsp-pyright :repo "emacs-lsp/lsp-pyright"
;;                           :host github)
;;   :ensure t
;;   :init
;;   (setq lsp-pyright-multi-root nil)
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))


(setenv "PYTHONIOENCODING" "utf8")

(provide 'sm-coding-python)
