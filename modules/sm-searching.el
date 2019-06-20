;; deadgrep
;; Uses ripgrep + nice results. This is replacing ag because ripgrep is faster
;; and the deadgrep interface is great.
(use-package deadgrep
  :bind ("C-c S" . deadgrep))

;; anzu
;; Shows isearch results in mode-line and better query-replace.
(use-package anzu
  :delight anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;; engine
;; Search engines integrated into Emacs.
(use-package engine-mode
  :commands (engine/search-github engine/search-google)
  :bind (("C-c / g" . engine/search-google)
         ("C-c / h" . engine/search-github))
  :config
  (setq engine/browser-function 'eww-browse-url)
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "h")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g"))

(provide 'sm-searching)
