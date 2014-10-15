;; Sane defaults.

;; Set default directory.
(setq default-directory "~")

;; Set home dir.
(cd (expand-file-name "~/"))

;; Disable GUI.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Never ring the bell. Never.
(setq ring-bell-function (lambda()))

;; Hide mouse while typing.
(setq make-pointer-invisible t)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically scroll compilation window.
(setq compilation-scroll-output 1)

;; Keep backups in a separate directory
(defun make-backup-file-name (file)
  (concat "~/.emacs.d/cache/backups" (file-name-nondirectory file) "~"))

;; Change auto-save-list directory.
(setq auto-save-list-file-prefix "~/.emacs.d/cache/auto-save-list/.saves-")

;; Change eshell directory.
(setq eshell-directory-name "~/.emacs.d/cache/eshell")

;; Since I use .emacs.d/cache as a temp directory, this checks whether
;; it exists and creates the directory if necessary.
(unless (file-exists-p "~/.emacs.d/cache/")
  (make-directory "~/.emacs.d/cache/"))

;; Do the same for .emacs.d/etc, which is used for settings files
;; and other similar stuff (keeping lib/ solely for .el files).
(unless (file-exists-p "~/.emacs.d/etc/")
  (make-directory "~/.emacs.d/etc/"))

;; Disable annoying lock files.
(setq create-lockfiles nil)

;; Change bookmarks file location.
(setq bookmark-file "~/.emacs.d/etc/bookmarks")

;; Change save-places file location.
(setq save-place-file "~/.emacs.d/cache/places")

;; GC optimisation.
;; Increases garbage collection threshold to 20mb (from 0.76mb)
(setq gc-cons-threshold 20000000)

;; Allow pasting selection outside of Emacs.
(setq x-select-enable-clipboard t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Misc.
(setq inhibit-startup-message t)
(setq color-theme-is-global t)
(global-font-lock-mode t)

(provide 'init-defaults)
