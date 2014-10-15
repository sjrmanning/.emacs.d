;; Various personal settings probably only applicable to my setup.

;; Org-mode personal settings.
(setq org-directory "~/Org")
(setq org-default-notes-file (concat org-directory "/Notes.org"))

;; Org-mode templates.
(setq org-capture-templates
      '(("w" "Work Task" entry (file+headline (concat org-directory "/Work.org") "Tasks")
         "* TODO %?\n %i\n")
        ("h" "Home Task" entry (file+headline (concat org-directory "/Home.org") "Tasks")
         "* TODO %?\n %i\n")
        ("n" "Note" entry (file+headline (concat org-directory "/Notes.org") "Captured")
         "* %^{Description} %T %^G\n %i%?\n %A")))

;; Use zsh as default term shell.
(setq explicit-shell-file-name "zsh")

(provide 'init-personal)
