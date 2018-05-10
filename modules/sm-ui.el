;;; sm-ui.el --- UI niceties.

;; Bind for toggling fullscreen.
(bind-key "C-c M-f" 'toggle-frame-fullscreen)

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(setq uniquify-buffer-name-style 'forward)

;; ido
(use-package ido
  :straight (flx-ido ido-completing-read+ ido-vertical-mode)
  :config
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (setq ido-enable-flex-matching t
        ido-enable-prefix nil
        ido-max-prospects 10
        ido-use-faces nil
        flx-ido-use-faces t
        ido-vertical-define-keys 'C-n-and-C-p-only
        ido-auto-merge-delay-time 99999999
        ido-everywhere t
        ido-virtual-buffers t)
  (ido-mode)
  (ido-vertical-mode)
  (ido-ubiquitous-mode)
  (flx-ido-mode))

;; (use-package flx-ido :config (flx-ido-mode))
;; (use-package ido-vertical-mode :config (ido-vertical-mode))

;; smex
(use-package smex
  :commands smex
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-x C-m" . smex)
         ("C-c C-m" . smex))
  :init (setq smex-save-file (sm/cache-for "smex-items"))
  :config (smex-initialize))

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
