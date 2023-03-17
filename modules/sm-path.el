;; Keep .emacs.d paths clean!
(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; `exec-path-from-shell' is kind of slow, so set up a reasonable PATH env
;; here. This may need some extension in the future to include rbenv support.
;; This is only needed when PATH isn't injected (e.g. with emacs-plus)
;; (setenv "PATH" "/opt/homebrew/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/Frameworks/Python.framework/Versions/3.9/bin")

(provide 'sm-path)
