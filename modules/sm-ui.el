;;; sm-ui.el --- UI niceties.

;; Bind for toggling fullscreen.
(bind-key "C-c M-f" 'toggle-frame-fullscreen)

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(setq uniquify-buffer-name-style 'forward)

;; Smooth scrolling.
(use-package smooth-scrolling
  :hook (after-init . smooth-scrolling-mode))

;; ivy everywhere
(use-package ivy
  :commands ivy-mode
  :hook (after-init . ivy-mode)
  :delight ivy-mode
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-immediate-done)
              ("RET" . ivy-alt-done))
  :config
  (ivy-prescient-mode t)
  (setq ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function nil
        ivy-use-selectable-prompt t
        enable-recursive-minibuffers t
        ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (t . ivy-prescient-re-builder))))

(use-package swiper
  :commands swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         (:map swiper-map
               ("C-r" . ivy-previous-line))))

(use-package counsel
  :after ivy
  :commands (counsel-M-x counsel-find-file counsel-rg)
  :delight counsel-mode
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c s" . counsel-rg))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-prescient
  :commands ivy-prescient-mode
  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy))
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (prescient-persist-mode t))

(use-package which-key
  :delight which-key-mode
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.5))

;; diminish some modes.
(use-package simple
  :straight nil
  :delight visual-line-mode)
(use-package abbrev
  :straight nil
  :delight abbrev-mode)

(provide 'sm-ui)
