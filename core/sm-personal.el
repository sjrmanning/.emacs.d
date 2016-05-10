;; Various personal settings probably only applicable to my setup.
;; Load private settings if found.
(require 'sm-defuns)
(defvar private-dir "~/.private/elisp"
  "Private elisp directory")
(if (file-exists-p private-dir)
    (sm/load-directory private-dir))

;; Use zsh as default term shell.
(setq explicit-shell-file-name "zsh")

(provide 'sm-personal)
