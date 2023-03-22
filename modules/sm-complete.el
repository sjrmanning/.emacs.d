;;; sm-complete.el --- Auto-completion.

;;; Commentary:

;; This file configures auto-completion niceties.

;;; Code:

(use-package corfu
  :straight
  (:files (:defaults "extensions/*")
          :includes (corfu-info corfu-history)
          :not autoloads)
  :hook (prog-mode shell-mode org-mode)
  :bind (:map corfu-map
	            ("C-n" . #'corfu-next)
              ("C-p" . #'corfu-previous)
              ("<escape>" . #'corfu-quit)
	            ("<return>" . #'corfu-insert)
	            ("M-d" . #'corfu-info-documentation)
              ("M-l" . #'corfu-info-location)
              ("M-n" . #'corfu-popupinfo-scroll-up)
              ("M-p" . #'corfu-popupinfo-scroll-down))
  :config
  (corfu-popupinfo-mode t)
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  (corfu-popupinfo-delay 0.05))

(use-package cape
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/"))
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-terminal
  :unless window-system
  :after corfu
  :hook (corfu-mode . corfu-terminal-mode))

;;; _
(provide 'sm-complete)
;;; sm-complete.el ends here
