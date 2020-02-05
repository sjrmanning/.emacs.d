;;; sm-coding-sh.el --- sh, bash, etc... config.

;; shell script support (bash, bats, etc...)
(use-package sh-script
  :straight nil
  :init
  :mode (("\\.bats\\'" . sh-mode)))

;; rips off ideas from ESS for use with the shell
(use-package essh
  :straight nil
  :load-path "etc/extra"
  :config
  (add-hook
   'sh-mode-hook
   (lambda ()
     (progn
       (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
       (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
       (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
       (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
       (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
       (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory)
       ))))

(provide 'sm-coding-sh)
