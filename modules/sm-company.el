;; company-mode
;; Auto-completion backend.
(use-package company
  :delight " ©"
  :commands (company-mode global-company-mode)
  :hook ((prog-mode comint-mode org-mode) . company-mode)
  :config
  ;; Company settings.
  (setq-default company-backends (remove 'company-dabbrev company-backends))
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.25)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (setq company-backends
        (mapcar #'sm/backend-with-yas company-backends)))

;; Quick-help (popup documentation for suggestions).
(use-package company-quickhelp
  :if window-system
  :after company
  :config (company-quickhelp-mode t))

;; Use company with LSP.
(use-package company-lsp
  :after (lsp-mode company)
  :config
  (push 'company-lsp company-backends))

(provide 'sm-company)
