;;; sm-coding-python.el --- Python setup.

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (use-package lsp-python-ms
    :init (require 'lsp-python-ms)
    :hook (python-mode . lsp)))

;; Clone yapf somewhere and throw a script like this onto your bin
;; (sigh...)
;;
;; #!/bin/sh
;; PYTHONPATH=$HOME/tmp/yapf python3 $HOME/tmp/yapf/yapf "$@"
(use-package yapfify
  :config
  (add-hook 'python-mode-hook 'yapf-mode))


(provide 'sm-coding-python)
