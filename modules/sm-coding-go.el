;;; sm-coding-go.el --- Golang config.

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save nil 'local)
  )

(use-package go-guru)

(provide 'sm-coding-go)
