;; Display related settings.

;; Default fonts.
(add-hook 'window-setup-hook
          (lambda nil
            (add-to-list 'default-frame-alist '(internal-border-width . 0))
            (set-fringe-mode '(8 . 0))
            (set-face-attribute
             'default nil
             :family "Source Code Pro"
             :height 130
             :weight 'normal)
            (set-face-attribute
             'linum nil
             :family "Source Code Pro"
             :height 120
             :weight 'normal)))

;; Disable cursor display in inactive windows.
(setq-default cursor-in-non-selected-windows nil)

;; Redraw without pause while processing input.
(setq redisplay-dont-pause t)

(provide 'init-look)
