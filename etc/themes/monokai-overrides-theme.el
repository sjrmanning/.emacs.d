(deftheme monokai-overrides)

(let ((class '((class color) (min-colors 257)))
      (terminal-class '((class color) (min-colors 89))))

  (custom-theme-set-variables
   'monokai-overrides

   `(pos-tip-background-color "#E6DB74")
   `(pos-tip-foreground-color "#272822"))

  (custom-theme-set-faces
   'monokai-overrides

   ;; default
   `(default
      ((,class (:foreground "#F8F8F2"
                            :background "#242728"))
       (,terminal-class (:foreground "#F8F8F2"
                                     :background "#242728"))))

   ;; Linum and mode-line improvements (only in sRGB).
   `(linum
     ((,class :foreground "#75715E"
              :background "#383c3d")))
   `(mode-line-inactive
     ((,class (:box (:line-width 8 :color "#2e3132" :style nil)
                    :background "#2e3132"))))
   `(mode-line
     ((,class (:box (:line-width 8 :color "#35393b" :style nil)
                    :background "#35393b"))))

   ;; Custom region colouring.
   `(region
     ((,class :foreground "#75715E"
              :background "#424748")
      (,terminal-class :foreground "#1B1E1C"
                       :background "#8B8878")))

   ;; smartparens
   `(sp-show-pair-match-face
     ((,class (:foreground "#AE81FF"
                           :background "#272822"
                           :weight normal
                           :inverse-video t))
      (,terminal-class (:foreground "#AF87FF"
                                    :background "#1B1E1C"
                                    :weight normal
                                    :inverse-video t))))

   ;; isearch
   `(isearch
     ((,class (:background "#349B8D"
                           :foreground "#BBF7EF"))
      (,terminal-class (:inherit region
                                 :background "#AF87D7"))))

   ;; monky
   `(monky-diff-title
     ((,class (:background "#3e3d31"))))

   ;; Additional modes
   ;; Company tweaks.
   `(company-tooltip-common
     ((t :foreground "#F8F8F0"
         :background "#474747"
         :underline t)))

   `(company-template-field
     ((t :inherit company-tooltip
         :foreground "#C2A1FF")))

   `(company-tooltip-selection
     ((t :background "#349B8D"
         :foreground "#BBF7EF")))

   `(company-tooltip-common-selection
     ((t :foreground "#F8F8F0"
         :background "#474747"
         :underline t)))

   `(company-scrollbar-fg
     ((t :background "#BBF7EF")))

   `(company-tooltip-annotation
     ((t :inherit company-tooltip
         :foreground "#C2A1FF")))

   ;; pos-tip
   `(tooltip
     ((t :inherit tooltip
         :foreground "#272822"
         :background "#E6DB74")))

   ;; Popup menu tweaks.
   `(popup-menu-face
     ((t :foreground "#A1EFE4"
         :background "#49483E")))

   `(popup-menu-selection-face
     ((t :background "#349B8D"
         :foreground "#BBF7EF")))

   ;; Circe
   `(circe-prompt-face
     ((t (:foreground "#C2A1FF" :weight bold))))

   `(circe-server-face
     ((t (:foreground "#75715E"))))

   `(circe-highlight-nick-face
     ((t (:foreground "#AE81FF" :weight bold))))

   `(circe-my-message-face
     ((t (:foreground "#E6DB74"))))

   `(circe-originator-face
     ((t (:weight bold))))))

(provide-theme `monokai-overrides)
