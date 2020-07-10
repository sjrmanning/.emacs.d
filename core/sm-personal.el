;; Various personal settings probably only applicable to my setup.
(require 'sm-defuns)

;; Main org and org-roam dirs.
;; The main notes directory is used for tasks and agenda-related items.
;; Roam is where actual note-taking/ideas/journaling lives.
(defvar sm/org-dir "~/notes")
(defvar sm/org-roam-dir "~/notes/roam")
(defvar sm/org-journal-dir "~/notes/roam/journal")

;; Load private settings if found.
(defvar private-dir "~/.private/elisp"
  "Private elisp directory")
(if (file-exists-p private-dir)
    (sm/load-directory private-dir))

;; Use zsh as default term shell.
(setq explicit-shell-file-name "zsh")

(provide 'sm-personal)
