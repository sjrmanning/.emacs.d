;;; sm-coding-go.el --- Golang config.

;; go get golang.org/x/tools/cmd/goimports
;; go get github.com/rogpeppe/godef

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            ;; (flycheck-mode)
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (local-set-key (kbd "M-*") 'pop-tag-mark))))

(use-package go-guru)

;;;;;;;

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (add-hook 'go-mode-hook #'lsp-deferred))

;; optional - provides fancier overlays
;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))
;; (use-package yasnippet)

;; if you use company-mode for completion (otherwise,
;; complete-at-point works out of the box):
;; (use-package company-lsp
;;   :commands company-lsp)

(provide 'sm-coding-go)

;; From http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/
;;; goflymake -- https://github.com/dougm/goflymake
;;; goerrcheck
