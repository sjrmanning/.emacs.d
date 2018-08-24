;;; sm-coding-go.el --- Golang config.

(use-package go-mode
  :commands go-mode
  :config
  (setq gofmt-command "goimports")
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(provide 'sm-coding-go)
