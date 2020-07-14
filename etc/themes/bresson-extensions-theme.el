(deftheme bresson-extensions
  "Extensions to the bresson core.")

(custom-theme-set-faces
 'bresson-extensions

 `(default ((t :foreground "#1a1a1a"))) ;;:background "#fefef0")))
 `(line-number ((t :background "#fffaef")))
 `(hl-line ((t :background "#f5f0e6")))
 `(minibuffer-prompt ((t :foreground "#3F4D91")))
 `(region ((t :background "#eee9d6")))
 `(mode-line-buffer-id ((t (:foreground nil))))
 `(mode-line
   ((t (:underline nil :overline nil
                   :foreground "#435459" ; 35% from 46% brightness
                   :box (:line-width 8 :color "#ededdf")))))
 `(mode-line-inactive
   ((t (:underline nil :overline nil
                   :background "#faf5eb"
                   :box (:line-width 8 :color "#faf5eb")))))

 `(font-lock-comment-face ((t (:foreground "#839496"))))
 `(font-lock-keyword-face ((t (:weight semibold))))
 `(font-lock-comment-delimiter-face ((t (:foreground "#aebfbf"))))

 )

(provide-theme 'bresson-extensions)
