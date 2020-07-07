;; Navigation related settings and binds.
(bind-key "C-x C-b" 'ibuffer)

(defun create-new-buffer ()
  "Create a new buffer named *new*."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*new*")))

(bind-key "C-c n" 'create-new-buffer)

(defun sm/smart-find-file ()
  "Find files using projectile if within a project, or fall-back to `find-file'."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (call-interactively 'find-file)))

(bind-key "C-x f" #'sm/smart-find-file)

(defun sm/kill-default-buffer ()
  "Kill the currently active buffer."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(bind-key "C-x k" 'sm/kill-default-buffer)

(defun switch-to-irc nil
  "Switch to IRC buffer."
  (interactive)
  (let ((final-list (list ))
        (irc-modes '(circe-channel-mode
                     circe-query-mode
                     erc-mode)))

    (dolist (buf (buffer-list) final-list)
      (if (member (with-current-buffer buf major-mode) irc-modes)
          (setq final-list (append (list (buffer-name buf)) final-list))))
    (when final-list
      (switch-to-buffer (completing-read "IRC Buffer: " final-list)))))

(defun sm/create-non-existent-directory ()
  "Prompt to automagically create parent directories."
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'sm/create-non-existent-directory)

;; switch-window
;; Provides visual cues to instantly switch on C-x o.
(use-package switch-window
  :bind ("C-x o" . switch-window))

;; saveplace
;; Remebers your location in a file when saving files.
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (sm/cache-for "saveplace")))

;; avy
;; A better jump to char and line.
(use-package avy
  :config (setq avy-style 'at)
  :bind (("C-o" . avy-goto-char)
         ("M-g" . avy-goto-line)))

;; recentf
;; Open/view recent files.
(use-package recentf
  :commands recentf-mode
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 200
        recentf-auto-cleanup 300
        recentf-exclude '("/TAGS$"
                          "/tmp/"
                          "/var/tmp/"
                          ".recentf"
                          "ido.last"
                          "/elpa/.*\\'"))
  (setq recentf-save-file (sm/cache-for "recentf"))
  (recentf-mode))

(provide 'sm-navigation)
