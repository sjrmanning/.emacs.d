;;; sm-appearance.el --- Display and appearance related settings.

;; Disable cursor display in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

(defvar sm/fixed-font-name "InconsolataGo Nerd Font Mono")
(defvar sm/fixed-font-weight 'regular)
(defvar sm/var-font-name "InconsolataGo Nerd Font Mono")
(defvar sm/font-height 160)

;; Native line numbers and fringe setup.
(setq-default display-line-numbers-width 4)

(use-package distinguished-theme)

(add-hook 'after-init-hook
          (lambda nil
            (set-fringe-style nil)
            ;; (setq-default cursor-type 'bar)
            (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
            (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
            (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
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
             :family sm/var-font-name))
          (if (display-graphic-p)
              ;; (load-theme 'wombat t)
              (load-theme 'distinguished t)
            ;; (load-theme 'heroku t)
            ;; dark theme, very contrasty.
            ;; (load-theme 'manoj-dark t)
            ;; (load-theme 'tsdh-dark t)
            ;; (load-theme 'wombat t)
            (load-theme 'distinguished t)
            ))

(use-package rainbow-mode
  :commands rainbow-mode)

;; Emacs on Mac OS X has a habit of opening new frames right on top of
;; existing frames, which make the older ones hard to get to.  This
;; just joggles new frames down and right a bit so they stack neatly.
(defun joggle (frame)
  (set-frame-parameter frame 'top
                       ;; make room for title bar
                       (+ (frame-parameter (selected-frame) 'top) 30))
  (set-frame-parameter frame 'left
                       (+ (frame-parameter (selected-frame) 'left) 15)))
(when window-system
  (add-hook 'after-make-frame-functions 'joggle))

(provide 'sm-appearance)
