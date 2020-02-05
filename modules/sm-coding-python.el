;;; sm-coding-python.el --- Python setup.

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (use-package lsp-python-ms
    :ensure t
    :init (require 'lsp-python-ms)
    :hook (python-mode . lsp-deferred)))

;; Clone yapf somewhere and throw a script like this onto your bin
;; (sigh...)
;;
;; #!/bin/sh
;; PYTHONPATH=$HOME/tmp/yapf python3 $HOME/tmp/yapf/yapf "$@"
;; (use-package yapfify
;;   :config
;;   (add-hook 'python-mode-hook 'yapf-mode))

;; (use-package python-black
;;   :demand t
;;   :after python
;;   :hook
;;   (
;;    (python-mode . python-black-on-save-mode)))

(use-package py-yapf
  :hook
  ((python-mode . py-yapf-enable-on-save)))


(setenv "PYTHONIOENCODING" "utf8")

(provide 'sm-coding-python)
