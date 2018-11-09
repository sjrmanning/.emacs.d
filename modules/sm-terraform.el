;;; sm-coding-go.el --- Golang config.

(use-package terraform-mode
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  )

(provide 'sm-terraform)
