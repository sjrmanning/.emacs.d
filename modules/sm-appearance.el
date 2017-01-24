;;; sm-appearance.el --- Display and appearance related settings.

;; Disable cursor display in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

;; Redraw without pause while processing input.
(setq redisplay-dont-pause t)

(defvar sm/fixed-font-name "Office Code Pro D")
(defvar sm/fixed-font-weight 'light)
(defvar sm/var-font-name "SF UI Text")
(defvar sm/font-height 100)

(add-hook 'emacs-startup-hook ;; 'window-setup-hook
          (lambda nil
            (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
            (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
            (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
            (run-with-idle-timer 0.1 nil (lambda nil (toggle-frame-maximized)))
            (set-fringe-mode '(8 . 0))
            (set-face-attribute
             'default nil
             :family sm/fixed-font-name
             :height sm/font-height
             :weight sm/fixed-font-weight)
            (set-face-attribute
             'linum nil
             :family sm/fixed-font-name
             :height (- sm/font-height 20)
             :weight sm/fixed-font-weight)
            (set-face-attribute
             'variable-pitch nil
             :family sm/var-font-name))
          (use-package darkokai-theme
            :config (load-theme 'darkokai t)))

(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'sm-appearance)
