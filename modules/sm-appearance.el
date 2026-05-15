;;; sm-appearance.el --- Display and appearance related settings.  -*- lexical-binding: t; -*-

;; Disable cursor display in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

(defvar sm/fixed-font-weight 'regular)
(defvar sm/fixed-font-name "Noto Sans Mono")
(defvar sm/var-font-name "Noto Sans")

;; ;; Quick workaround to help switching between retina / 1440p.
(defvar sm/dynamic-font-height t)
(defvar sm/font-height
  (cond ((eq sm/dynamic-font-height t)
         (if (< (display-pixel-height) 1600) 160 130))
        (t 150)))

;; ;; Appearance style can be light or dark.
;; ;; Setting this here swaps between themes
(defvar sm/appearance-style 'light)

;; ;; Native line numbers and fringe setup.
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
             'fixed-pitch nil
             :family sm/fixed-font-name
             :height sm/font-height
             :weight sm/fixed-font-weight)
            (set-face-attribute
             'line-number nil
             :family sm/fixed-font-name
             :height sm/font-height
             :weight 'extra-light)
            (set-face-attribute
             'variable-pitch nil
             :family sm/var-font-name
             :weight 'regular)))

(add-to-list 'custom-theme-load-path
             (expand-file-name "etc/themes/" user-emacs-directory))

(use-package os1-theme
  :defer nil
  :ensure (:type git :host github :repo "sashimacs/os1-theme")
  :custom
  (os1-prefer-semibold t)
  :config
  (load-theme 'os1 t))
(elpaca-wait)

(provide 'sm-appearance)
