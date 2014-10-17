;; Display related settings.

;; Window setup.
(add-hook 'window-setup-hook
          (lambda nil
            (run-with-idle-timer 0.1 nil (lambda nil (toggle-frame-maximized)))
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
