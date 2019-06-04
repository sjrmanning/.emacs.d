;;; sm-coding-go.el --- Golang config.

;; go get golang.org/x/tools/cmd/goimports
;; go get github.com/rogpeppe/godef

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook (lambda ()
                            (local-set-key (kbd "M-.") 'godef-jump)
                            (local-set-key (kbd "M-*") 'pop-tag-mark))))

(use-package go-guru)

(provide 'sm-coding-go)

;; From http://dominik.honnef.co/posts/2013/03/writing_go_in_emacs/
;;; goflymake -- https://github.com/dougm/goflymake
;;; goerrcheck
