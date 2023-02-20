;;; sm-ui.el --- UI niceties.

;; Bind for toggling fullscreen.
(bind-key "C-c M-f" 'toggle-frame-fullscreen)

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(setq uniquify-buffer-name-style 'forward)

(use-package gcmh
  :delight (gcmh-mode)
  :commands (gcmh-mode)
  :functions (gcmh-idle-garbage-collect)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-verbose nil)
  :hook
  (emacs-startup . gcmh-mode))

;; Smooth scrolling.
(use-package smooth-scrolling
  :hook (after-init . smooth-scrolling-mode))

;; Vertico / orderless / marginalia et al.
(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  :init
  (marginalia-mode))

(use-package all-the-icons)

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (;; vertico-indexed
                                ;; vertico-flat
                                ;; vertico-grid
                                ;; vertico-mouse
                                ;; vertico-quick
                                ;; vertico-buffer
                                ;; vertico-repeat
                                ;; vertico-reverse
                                vertico-directory
                                ;; vertico-multiform
                                ;; vertico-unobtrusive
                                ))
  :init (vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package savehist
  :hook (after-init . savehist-mode)
  :custom (savehist-file (no-littering-expand-var-file-name "savehist.el"))
  :config
  (setq savehist-autosave-interval nil
        savehist-additional-variables
        '(register-alist
          mark-ring global-mark-ring
          search-ring regexp-search-ring)))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     orderless-flex)))

(use-package consult
  :custom (consult-project-function #'projectile-project-root)
  :bind (("C-c s" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)))

(use-package consult-projectile)

(use-package swiper
  :commands swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))

(use-package which-key
  :delight
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 1.0))

;; diminish some modes.
(use-package simple
  :straight nil
  :delight visual-line-mode)
(use-package abbrev
  :straight nil
  :delight abbrev-mode)

(provide 'sm-ui)
