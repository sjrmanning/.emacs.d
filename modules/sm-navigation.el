;; Navigation related settings and binds.
(use-package ibuffer
  :straight (:type built-in)
  :bind ("C-x C-b" . ibuffer))

(use-package files
  :straight (:type built-in)
  :bind (("C-x f" . sm/smart-find-file)
         ("C-x k" . sm/kill-default-buffer))
  :init
  (defun sm/smart-find-file ()
    "Find files using projectile if within a project, or fall-back to `find-file'."
    (interactive)
    (if (projectile-project-p)
        (consult-projectile-find-file)
      (call-interactively 'find-file)))

  (defun sm/kill-default-buffer ()
    "Kill the currently active buffer."
    (interactive)
    (let (kill-buffer-query-functions) (kill-buffer)))

  (defun sm/create-non-existent-directory ()
    "Prompt to automagically create parent directories."
    (let ((parent-directory (file-name-directory buffer-file-name)))
      (when (and (not (file-exists-p parent-directory))
                 (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
        (make-directory parent-directory t))))
  (add-to-list 'find-file-not-found-functions #'sm/create-non-existent-directory))

;; switch-window
;; Provides visual cues to instantly switch on C-x o.
(use-package switch-window
  :straight (:build (:not autoloads))
  :commands switch-window
  :bind ("C-x o" . switch-window))

;; saveplace
;; Remebers your location in a file when saving files.
(use-package saveplace
  :hook (after-init . save-place-mode)
  :custom (save-place-file (sm/cache-for "saveplace")))

;; avy
;; A better jump to char and line.
(use-package avy
  :bind ("C-o" . avy-goto-char)
  :custom (avy-style 'at))

;; recentf
;; Open/view recent files.
(use-package recentf
  :hook after-init
  :custom
  (recentf-auto-cleanup 600)
  (recentf-max-saved-items 200)
  (recentf-exclude '("/TAGS$"
                     "/tmp/"
                     "/var/tmp/"
                     ".recentf"
                     "ido.last"
                     "/elpa/.*\\'"))
  (recentf-save-file (sm/cache-for "recentf")))

(provide 'sm-navigation)
