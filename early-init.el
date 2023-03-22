;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; For native-comp to work ensure LIBRARY_PATH has macOS SDK from Xcode.
(setenv "LIBRARY_PATH" (concat (substring (shell-command-to-string "/usr/bin/xcrun --show-sdk-path") 0 -1) "/usr/lib"))

;; GCC Emacs optimization setting.
(setq native-comp-speed 2)

;; Using straight means we don't want to initialize package.el at all.
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(fset 'display-startup-echo-area-message 'ignore)
(setq inhibit-startup-buffer-menu t)
