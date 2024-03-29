;; Sane defaults.
(require 'sm-defuns)

;; Ignore customisation by putting it in the cache dir.
(setq custom-file (sm/cache-for "custom.el"))

;; Set default directory.
(setq default-directory "~")

;; Set home dir.
(cd (expand-file-name "~/"))

;; Mac defaults.
(when (equal system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (global-set-key [(super q)] 'save-buffers-kill-emacs))

;; Frame title formatting.
(setq-default frame-title-format
              '((:eval (if (buffer-file-name)
                           (abbreviate-file-name (buffer-file-name))
                         "%b"))))

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Show column numbers in mode line.
(setq column-number-mode t)

;; Never ring the bell. Never.
(setq ring-bell-function (lambda()))

;; Don't disable any commands (e.g. `upcase-region').
(setq disabled-command-function nil)

;; Don't use dialog boxes.
(setq use-dialog-box nil)

;; Hide mouse while typing.
(setq make-pointer-invisible t)

;; Reduce keystroke echo delay.
(setq echo-keystrokes 0.001)

;; Enable y/n answers.
(setq use-short-answers t)

;; Automatically scroll compilation window.
(setq-default compilation-scroll-output t)

;; Emoji font.
(set-fontset-font t 'symbol
                  (font-spec :family "Apple Color Emoji")
                  nil 'prepend)

;; Keep autosave files in /tmp.
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Change auto-save-list directory.
(setq auto-save-list-file-prefix (sm/cache-for "auto-save-list/.saves-"))

;; Change eshell directory.
(setq-default eshell-directory-name (sm/cache-for "eshell"))

;; Disable annoying lock files.
(setq create-lockfiles nil)

;; Change bookmarks file location.
(setq-default bookmark-default-file (sm/emacs.d "etc/bookmarks"))

;; Allow pasting selection outside of Emacs.
(setq-default x-select-enable-clipboard t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Ignore case for completion.
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; When copying something from outside emacs, save to kill-ring.
(setq save-interprogram-paste-before-kill t)

;; Dired defaults.
(setq-default insert-directory-program "gls")
(setq-default dired-listing-switches "-lhva")
(setq-default dired-clean-up-buffers-too t)
(setq-default dired-recursive-copies 'always)
(setq-default dired-recursive-deletes 'top)

;; Scratch buffer configuration.
(setq initial-major-mode 'markdown-mode)
(setq initial-scratch-message "# ")

;; Highlight lines.
(global-hl-line-mode t)

;; Misc.
(setq inhibit-startup-message t)
(global-font-lock-mode t)

(provide 'sm-defaults)
