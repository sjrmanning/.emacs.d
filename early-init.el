;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; GCC Emacs optimization setting.
(setq-default native-comp-speed 3)

;; Using straight means we don't want to initialize package.el at all.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Allow Emacs to resize per-pixel, not per character width.
(setq frame-resize-pixelwise t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Replace startup message with init-time.
(fset 'display-startup-echo-area-message
      (lambda () (message (concat "Loaded config in " (emacs-init-time)))))

;; Don't show buffer menu when opening multiple files.
(setq inhibit-startup-buffer-menu t)
