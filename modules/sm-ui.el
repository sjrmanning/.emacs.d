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
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (t      . ivy-prescient-re-builder)))
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-j" . ivy-immediate-done)
         ("RET" . ivy-alt-done)))

(use-package counsel
  :hook (after-init . counsel-mode)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c s" . counsel-ag)))

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
  :defer 10
  :config
  (mouse-avoidance-mode 'exile))

(provide 'sm-ui)
