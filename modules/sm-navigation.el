;; switch-window
;; Provides visual cues to instantly switch on C-x o.
(use-package switch-window
  :bind ("C-x o" . switch-window))

;; saveplace
;; Remebers your location in a file when saving files.
(use-package saveplace
  :init
  (setq save-place-file (sm/cache-for "saveplace"))
  (setq-default save-place t))

;; avy
;; A better jump to char and line.
(use-package avy
  :config (setq avy-style 'at)
  :bind (("C-o" . avy-goto-char)
         ("M-g" . avy-goto-line)))

;; smooth-scrolling
;; Avoids annoying behaviour when scrolling past the edges of a buffer.
(use-package smooth-scrolling
  :init (smooth-scrolling-mode t))

;; recentf
;; Open/view recent files.
(use-package recentf
  :commands ido-recentf-open
  :bind ("C-x C-r" . ido-recentf-open)
  :config
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 200
        recentf-auto-cleanup 300
        recentf-exclude '("/TAGS$"
                          "/var/tmp/"
                          ".recentf"
                          "ido.last"
                          "/elpa/.*\\'"))
  (setq recentf-save-file (sm/cache-for "recentf"))
  (recentf-mode))

(provide 'sm-navigation)
