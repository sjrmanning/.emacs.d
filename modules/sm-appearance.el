;;; sm-appearance.el --- Display and appearance related settings.

;; Disable cursor display in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

(defvar sm/fixed-font-name "iA Writer Mono V")
(defvar sm/fixed-font-weight 'regular)
(defvar sm/var-font-name "iA Writer Quattro V")
(defvar sm/font-height 160)

;; Appearance style can be light or dark.
;; Setting this here swaps between themes
(defvar sm/appearance-style 'light)

;; Native line numbers and fringe setup.
(setq-default display-line-numbers-width 4)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(add-hook 'after-init-hook
          (lambda nil
            (set-fringe-style 0)
            (setq-default cursor-type 'bar)
            (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
            (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
            (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
            (run-with-idle-timer 0.1 nil (lambda nil (toggle-frame-maximized)))
            (set-face-attribute
             'default nil
             :family sm/fixed-font-name
             :height sm/font-height
             :weight sm/fixed-font-weight)
            (set-face-attribute
             'line-number nil
             :family sm/fixed-font-name
             :height (- sm/font-height 10)
             :weight sm/fixed-font-weight)
            (set-face-attribute
             'variable-pitch nil
             :family sm/var-font-name)))

(cond
 ;; Light style
 ((eq sm/appearance-style 'light)
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (use-package solarized-theme
    :custom
    (solarized-distinct-doc-face t)
    :custom-face
    (default ((t :foreground "#53676d")))
    (hl-line ((t :background "#F8F0D8")))
    (minibuffer-prompt ((t :foreground "#3F4D91")))
    (region ((t :background "#EAE3CB")))
    (mode-line
     ((t (:underline nil
                     :box (:line-width 8 :color "#eee8d5")))))
    (mode-line-inactive
     ((t (:underline nil :overline nil
                     :box (:line-width 8 :color "#fdf6e3")))))
    :config (load-theme 'solarized-light t)))

 ;; Dark style
 ((eq sm/appearance-style 'dark)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (use-package darkokai-theme
    :config
    (setq-default darkokai-blue-tint t)
    (load-theme 'darkokai t))))

(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'sm-appearance)
