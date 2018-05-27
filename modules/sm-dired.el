;;; sm-dired.el --- Dired addons and etc...
;;; See https://github.com/Fuco1/dired-hacks for other potentially
;;; useful hacks...

(use-package dired-subtree
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(provide 'sm-dired)
