;;; sm-notes.el --- Note-taking, org-mode, deft, et al.

;; ;; Fix org version warning.
;; (straight-override-recipe
;;  '(org :type git :host github :repo "emacsmirror/org"))

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
  :straight (:type built-in)
  :bind (("C-c C-x C-s" . mark-done-and-archive)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :hook (org-mode . org-indent-mode)
  :config
  ;; Enable company, mainly for org-roam.
  (company-mode t)
  ;; Follow links in the same window.
  (setcdr (assoc 'file org-link-frame-setup) 'find-file)
  (setq org-catch-invisible-edits 'show-and-error
        org-cycle-separator-lines 1
        org-hide-emphasis-markers t
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
  :init (setq org-roam-v2-ack t)
  :commands (org-roam-buffer-toggle-display
             org-roam-insert
             org-roam-find-file
             org-roam-switch-to-buffer)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph-show)
   ("C-c n i" . org-roam-insert)
   ("C-c n I" . org-roam-insert-immediate)
   ("C-c n b" . org-roam-switch-to-buffer)
   ("C-c n t" . org-roam-dailies-goto-today))
  :custom
  (org-roam-buffer-window-parameters '((no-delete-other-windows . t)))
  (org-roam-directory sm/org-roam-dir)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-completion-system 'ivy)
  (org-roam-dailies-directory "journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y-%m-%d>.org"
                         "#+title: %<%A, %Y-%m-%d>\n\n")
      :unnarrowed t)))
  :config
  (add-hook 'org-roam-buffer-prepare-hook (lambda () (setq mode-line-format nil))))

(use-package deft
  :custom (deft-directory sm/org-roam-dir))

(provide 'sm-notes)
