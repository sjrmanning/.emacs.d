;;; sm-ui.el --- UI niceties.

;; Bind for toggling fullscreen.
(bind-key "C-c M-f" 'toggle-frame-fullscreen)

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(setq uniquify-buffer-name-style 'forward)

;; smex support in counsel
(use-package smex
  :hook (after-init . smex-initialize))

;; fuzzy matching and better sorting for ivy.
(use-package ivy-prescient
  :hook (after-init . ivy-prescient-mode)
  :config
  (setq prescient-filter-method 'fuzzy))

;; ivy everywhere
(use-package ivy
  :commands ivy-mode
  :hook (after-init . ivy-mode)
  :delight ivy-mode
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-j" . ivy-immediate-done)
         ("RET" . ivy-alt-done))
  :config
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function nil
        ivy-use-selectable-prompt t
        enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (t      . ivy-prescient-re-builder))))

(use-package counsel
  :hook (after-init . counsel-mode)
  :delight counsel-mode
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c s" . counsel-rg)))

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
