;;; sm-ui.el --- UI niceties.

;; Bind for toggling fullscreen.
(bind-key "C-c M-f" 'toggle-frame-fullscreen)

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(setq uniquify-buffer-name-style 'forward)

(use-package prescient
  :hook
  (after-init . prescient-persist-mode))
(use-package selectrum-prescient
  :hook
  (after-init . selectrum-prescient-mode))

(use-package selectrum
  :hook
  (after-init . selectrum-mode)
  )

;; smex support in counsel
(use-package smex
  :hook (after-init . smex-initialize))

;; diminish some modes.
(use-package simple
  :straight nil
  :delight visual-line-mode)
(use-package abbrev
  :straight nil
  :delight abbrev-mode)

;; get rid of the mouse.
(use-package avoid
  :if window-system
  :config
  (mouse-avoidance-mode 'exile))

;; Extends the functionality of hl-line, I'm particularly interested
;; in only highlighting the line whilst I'm idle.
;; (use-package hl-line-plus
;;   :init
;;   (hl-line-when-idle-interval 10)
;;   (toggle-hl-line-when-idle 1))

(provide 'sm-ui)
