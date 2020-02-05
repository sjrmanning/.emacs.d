;; Keep .emacs.d paths clean!
(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Use $PATH from user's shell in Emacs.
(use-package exec-path-from-shell
  :if (memq window-system (quote (mac ns)))
  :defer 0.5
  :config
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(provide 'sm-path)
