;;; sm-appearance.el --- Display and appearance related settings.

;; Disable cursor display in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

(defvar sm/fixed-font-weight 'normal)
(defvar sm/fixed-font-name "Fira Code")
(defvar sm/var-font-name "iA Writer Quattro V")

;; Quick workaround to help switching between retina / 1440p.
(defvar sm/dynamic-font-height t)
(defvar sm/font-height
  (cond ((eq sm/dynamic-font-height t)
         (if (< (display-pixel-height) 1600) 160 130))
        (t 150)))

;; Appearance style can be light or dark.
;; Setting this here swaps between themes
(defvar sm/appearance-style 'light)

;; Native line numbers and fringe setup.
(setq-default display-line-numbers-width 4)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(add-hook 'after-init-hook
          (lambda nil
            ;; Enable macOS ligatures in fonts if running emacs-mac port.
            (when (fboundp 'mac-auto-operator-composition-mode)
              (mac-auto-operator-composition-mode))
            (setq-default cursor-type 'bar)
            (toggle-frame-maximized)
            (set-face-attribute
             'default nil
             :family sm/fixed-font-name
             :height sm/font-height
             :weight sm/fixed-font-weight)
            (set-face-attribute
             'line-number nil
             :family sm/fixed-font-name
             :height sm/font-height
             :weight sm/fixed-font-weight)
            (set-face-attribute
             'variable-pitch nil
             :family sm/var-font-name)))

(add-to-list 'custom-theme-load-path
             (expand-file-name "etc/themes/" user-emacs-directory))

(cond
 ;; Light style
 ((eq sm/appearance-style 'light)
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (use-package solarized-theme
    :defer nil
    :custom (solarized-distinct-doc-face t)
    :config
    (load-theme 'bresson t)
    (load-theme 'bresson-extensions t)))

 ;; Dark style
 ((eq sm/appearance-style 'dark)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (use-package darkokai-theme
    :defer nil
    :config
    (setq-default darkokai-blue-tint t)
    (load-theme 'darkokai t))))

(provide 'sm-appearance)
