;;; sm-notes.el --- Note-taking, org-mode, deft, et al.

;; Fix org version warning.
(straight-override-recipe
 '(org :type git :host github :repo "emacsmirror/org" :no-build t))

(setq org-directory sm/org-dir)

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(defun sm/find-orgfile ()
  "Use `completing-read' to \\[find-file] an org file."
  (interactive)
  (if (find-file (completing-read "Open org file: " org-agenda-files))
      (message "Opening file...")
    (message "Aborting")))

(defvar sm/org-basic-task-template "* TODO %?\n%U"
  "Basic task data")
(defvar sm/org-work-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
%U
%?

%i
" "Estimated task data")

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
  (company-mode t)
  (setq org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 0
        org-use-speed-commands t
        org-startup-indented t
        org-return-follows-link t
        org-hide-leading-stars t
        org-ellipsis "⤵"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
        org-log-done 'time
        org-default-notes-file (org-file-path "inbox.org")
        org-archive-location (concat
                              (org-file-path "archive.org") "::* From %s")

        org-agenda-files (list (org-file-path "inbox.org")
                               (org-file-path "next.org")
                               (org-file-path "projects.org")
                               (org-file-path "someday.org")
                               (org-file-path "reading.org"))

        org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)"
        org-agenda-custom-commands `(("A" "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "To Refile")
                                              (org-agenda-files '(,(org-file-path "inbox.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(org-file-path "someday.org")
                                                                  ,(org-file-path "projects.org")
                                                                  ,(org-file-path "next.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(org-file-path "next.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))

        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '(("next.org" :level . 0)
                             ("projects.org" :maxlevel . 3)
                             ("someday.org" :level . 0)
                             ("reading.org" :maxlevel . 2))

        org-tag-alist (quote (("@work" . ?w)
                              ("@home" . ?h)
                              (:newline)
                              ("WAITING" . ?W)
                              ("HOLD" . ?H)
                              ("CANCELLED" . ?c)))

        org-capture-templates
        `(("i" "Todo" entry
           (file ,(org-file-path "inbox.org"))
           ,sm/org-basic-task-template :empty-lines 1)

          ("e" "Estimated todo" entry
           (file ,(org-file-path "inbox.org"))
           ,sm/org-work-task-template :empty-lines 1)

          ("r" "Reading" todo ""
           (file ,(org-file-path "reading.org"))
           ,sm/org-basic-task-template :empty-lines 1))))

;; org-roam for capturing and organizing notes.
(use-package org-roam
  :hook (org-roam-backlinks-mode . turn-on-visual-line-mode)
  :hook (org-load . org-roam-mode)
  :commands (org-roam-buffer-toggle-display
             org-roam-insert
             org-roam-find-file
             org-roam-switch-to-buffer)
  :bind
  (("C-c n f" . org-roam-find-file)
   ("C-c n g" . org-roam-graph-show)
   ("C-c n i" . org-roam-insert)
   ("C-c n I" . org-roam-insert-immediate)
   ("C-c n b" . org-roam-switch-to-buffer))
  :custom
  (org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  (org-roam-directory sm/org-roam-dir)
  (org-roam-capture-templates
   '(("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${slug}"
      :unnarrowed t)))
  (org-roam-completion-system 'ivy)
  :config
  (add-hook 'org-roam-buffer-prepare-hook (lambda () (setq mode-line-format nil))))

(use-package company-org-roam
  :after (org-roam)
  :config (push 'company-org-roam company-backends))

;; For temporary notes and journaling.
(use-package org-journal
  :commands (org-journal-new-entry org-journal-today)
  :bind
  ("C-c n j" . org-journal-new-entry)
  ("C-c n t" . org-journal-today)
  :config
  (setq org-journal-date-prefix "#+title: "
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %Y-%m-%d"
        org-journal-dir sm/org-journal-dir
        org-journal-carryover-items nil)
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(use-package deft
  :custom (deft-directory sm/org-roam-dir))

(provide 'sm-notes)
