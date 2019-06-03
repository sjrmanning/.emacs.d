;;; sm-org.el --- Org-mode and related config.

(setq sm/org-dir "~/Documents/org")

;; Fix org version warning.
(straight-override-recipe
 '(org :type git :host github :repo "emacsmirror/org" :no-build t))

;; Pretty bullets.
(use-package org-bullets
  :commands org-bullets-mode
  :init (add-hook 'org-mode-hook #'org-bullets-mode))

(use-package org
  :bind (("C-c C-x C-s" . mark-done-and-archive)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :init (add-hook 'org-mode-hook #'org-indent-mode)
  :config
  ;; Functions and custom vars.
  (defvar sm/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")

  (defun org-file-path (filename)
    "Return the absolute address of an org file, given its relative name."
    (concat (file-name-as-directory org-directory) filename))

  (defun mark-done-and-archive ()
    "Mark the state of an org-mode item as DONE and archive it."
    (interactive)
    (org-todo 'done)
    (org-archive-subtree))

  (defun sm/ido-find-orgfile ()
    "Use `ido-completing-read' to \\[find-file] an org file."
    (interactive)
    (if (find-file (ido-completing-read "Open org file: " org-agenda-files))
        (message "Opening file...")
      (message "Aborting")))

  (setq org-directory sm/org-dir
        org-use-speed-commands t
        org-completion-use-ido t
        org-ellipsis "⤵"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-log-done 'time
        org-default-notes-file (org-file-path "index.org")
        org-archive-location (concat
                              (org-file-path "archive.org") "::* From %s")
        org-agenda-files (list (org-file-path "index.org")
                               (org-file-path "work.org")
                               (org-file-path "personal.org"))

        org-capture-templates
        `(("w" "Work-related tasks" entry
           (file+headline (org-file-path "work.org") "Inbox")
           ,sm/org-basic-task-template)

          ("p" "Personal tasks" entry
           (file+headline (org-file-path "personal.org") "Inbox")
           ,sm/org-basic-task-template)

          ("r" "Reading" checkitem
           (file+headline (org-file-path "index.org") "Reading"))

          ("n" "Notes and ideas" entry
           (file+headline (org-file-path "index.org") "Notes")))))

;; deft
(use-package deft
  :commands deft
  :bind ("M-<f1>" . deft)
  :config
  (setq deft-extension "org"
        deft-directory (concat sm/org-dir "/deft")
        deft-text-mode 'org-mode
        deft-use-filename-as-title t
        deft-auto-save-interval 30.0))

(provide 'sm-org)
