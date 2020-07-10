;; company-mode
;; Auto-completion backend.
(use-package company
  :delight " ©"
  :commands (company-mode global-company-mode)
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'comint-mode-hook #'company-mode)
  (add-hook 'org-mode-hook #'company-mode)
  :config
  ;; Quick-help (popup documentation for suggestions).
  (use-package company-quickhelp
    :if window-system
    :init (company-quickhelp-mode 1))

  ;; Use company with LSP.
  (use-package company-lsp
    :init
    (push 'company-lsp company-backends))

  ;; Company settings.
  (setq-default company-backends (remove 'company-eclim company-backends))
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.25)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq company-backends (remove 'company-clang company-backends))
  (setq company-backends
        (mapcar #'sm/backend-with-yas company-backends)))

(provide 'sm-company)
