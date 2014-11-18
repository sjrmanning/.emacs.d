(deftheme monokai-overrides)

(custom-theme-set-faces
 'monokai-overrides

 ;; Minor tweaks
 '(linum
   ((t :foreground "#75715E"
       :background "#49483E")))
 '(mode-line-inactive
   ((t (:box (:line-width 1 :color "#2c2d26" :style nil)
             :background "#2c2d26"))))

 ;; Additional modes
 ;; Circe
 '(circe-prompt-face
   ((t (:foreground ,violet-l :weight bold))))

 '(circe-server-face
   ((t (:foreground ,monokai-comments))))

 '(circe-highlight-nick-face
   ((t (:foreground ,violet :weight bold))))

 '(circe-my-message-face
   ((t (:foreground ,yellow))))

 '(circe-originator-face
   ((t (:weight bold)))))

(provide-theme 'monokai-overrides)
