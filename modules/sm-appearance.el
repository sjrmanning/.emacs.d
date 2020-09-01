;;; sm-appearance.el --- Display and appearance related settings.

;; Disable cursor display in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

(defvar sm/fixed-font-name "Offlig D")
(defvar sm/fixed-font-weight 'normal)
(defvar sm/var-font-name "iA Writer Quattro V")

;; Quick workaround to help switching between retina 13" and 27" 1440p.
(defvar sm/font-height
  (if (< (display-pixel-height) 1600)
      150 130))

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
            (set-fringe-style 0)
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
             ;; :height (- sm/font-height 20)
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

(use-package rainbow-mode)

;; When using Fira Code, this enables ligatures without mac-port.
(unless (fboundp 'mac-auto-operator-composition-mode)
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(provide 'sm-appearance)
