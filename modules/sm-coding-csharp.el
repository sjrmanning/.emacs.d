;;; sm-coding-csharp.el --- C# with Omnisharp configuration.

(use-package csharp-mode
  :mode "\\.cs$"
  :config
  ;; Omnisharp (C# completion, refactoring, etc.)
  (use-package omnisharp
    :commands omnisharp-mode
    :init
    (add-hook 'csharp-mode-hook
              (lambda ()
                (omnisharp-mode)
                (add-to-list 'company-backends (sm/backend-with-yas
                                                'company-omnisharp))))
    :config
    (setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")))

(provide 'sm-coding-csharp)
