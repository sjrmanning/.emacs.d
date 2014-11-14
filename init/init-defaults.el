;; Sane defaults.
(require 'init-defuns)

;; Set default directory.
(setq default-directory "~")

;; Set home dir.
(cd (expand-file-name "~/"))

;; Disable GUI.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Fix mac-port defaults.
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (global-set-key [(super v)] 'yank)
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
(setq echo-keystrokes 0.1)

;; Enable y/n answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically scroll compilation window.
(setq compilation-scroll-output 1)

(sm/mkdir-p (sm/emacs.d "cache"))
(sm/mkdir-p (sm/emacs.d "etc"))
(sm/mkdir-p (sm/emacs.d "cache/backups"))

;; Keep backups in a separate directory.
(defun make-backup-file-name (file)
  (concat (sm/emacs.d "cache/backups/") (file-name-nondirectory file) "~"))

;; Change auto-save-list directory.
(setq auto-save-list-file-prefix (sm/emacs.d "cache/auto-save-list/.saves-"))

;; Change eshell directory.
(setq eshell-directory-name (sm/emacs.d "cache/eshell"))

;; Disable annoying lock files.
(setq create-lockfiles nil)

;; Change bookmarks file location.
(setq bookmark-file (sm/emacs.d "etc/bookmarks"))

;; Change save-places file location.
(setq save-place-file (sm/emacs.d "cache/places"))

;; GC optimisation.
;; Increases garbage collection threshold to 50mb (from 0.76mb)
(setq gc-cons-threshold 50000000)

;; Allow pasting selection outside of Emacs.
(setq x-select-enable-clipboard t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Misc.
(setq inhibit-startup-message t)
(global-font-lock-mode t)

(provide 'init-defaults)
