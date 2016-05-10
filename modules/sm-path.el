;; Use $PATH from user's shell in Emacs.
(use-package exec-path-from-shell
  :if (memq window-system (quote (mac ns)))
  :defer 2
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

(provide 'sm-path)
