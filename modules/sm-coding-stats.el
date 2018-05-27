;;; sm-coding-stats.el --- support for statistics package & languages

;; There's surely more to be done here....
(use-package ess
  :commands R-mode
  :mode (("\\.R$" . R-mode)
         ("\\.r$" . R-mode)))

(provide 'sm-coding-stats)
