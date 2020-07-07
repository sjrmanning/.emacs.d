;;; sm-ui.el --- UI niceties.

;; Bind for toggling fullscreen.
(bind-key "C-c M-f" 'toggle-frame-fullscreen)

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(setq uniquify-buffer-name-style 'forward)

;; Smooth scrolling.
(setq hscroll-step 1
      scroll-conservatively 1000)

(use-package ctrlf
  :config (ctrlf-mode t))

(use-package counsel
  :commands (counsel-rg)
  :delight counsel-mode
  :bind
  (("C-c s" . counsel-rg)))

;; selectrum
(use-package selectrum
  :hook (after-init . selectrum-mode)
  :config
  (use-package selectrum-prescient
    :custom
    (prescient-filter-method '(literal regexp initialism fuzzy))

    :config
    (selectrum-prescient-mode t)
    (prescient-persist-mode t)))

;; diminish some modes.
(use-package simple
  :straight nil
  :delight visual-line-mode)
(use-package abbrev
  :straight nil
  :delight abbrev-mode)

(provide 'sm-ui)
