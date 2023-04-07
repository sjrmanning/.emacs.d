;; Keep .emacs.d paths clean!
(use-package no-littering
  :custom (auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; `exec-path-from-shell' is kind of slow, so set up a reasonable PATH env
;; here. This may need some extension in the future to include rbenv support.
;; This is only needed when PATH isn't injected (e.g. with emacs-plus)

;; Alternatively, you can inject PATH manually:
;; plutil -insert LSEnvironment -xml '<dict/>' /Applications/Emacs.app/Contents/Info.plist
;; plutil -insert LSEnvironment.PATH -string "$(printenv PATH)" /Applications/Emacs.app/Contents/Info.plist
;; /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -f /Applications/Emacs.app

;; (setenv "PATH" "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/homebrew/bin")
;; (add-to-list 'exec-path "/opt/homebrew/bin")
;; (add-to-list 'exec-path "/Library/Frameworks/Python.framework/Versions/3.9/bin")

(provide 'sm-path)
