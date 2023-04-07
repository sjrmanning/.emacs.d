;; deadgrep
;; Uses ripgrep + nice results. This is replacing ag because ripgrep is faster
;; and the deadgrep interface is great.
(use-package deadgrep
  :bind ("C-c S" . deadgrep))

;; anzu
;; Shows isearch results in mode-line and better query-replace.
(use-package anzu
  :diminish
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

(provide 'sm-searching)
