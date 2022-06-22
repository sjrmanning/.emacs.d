;; Keep .emacs.d paths clean!
(use-package no-littering
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; `exec-path-from-shell' is kind of slow, so set up a reasonable PATH env
;; here. This may need some extension in the future to include rbenv support.
(setenv "PATH" "/opt/brew/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")

(provide 'sm-path)
