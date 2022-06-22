;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; GCC Emacs deferred compilation.
;; Straight will native-compile packages unless specified not to.
;; I'm disabling deferred-compilation here since it will try to native-compile
;; packages even if you've explicitly avoided native-compilation via straight.
(setq native-comp-speed 2
      comp-deferred-compilation nil)
(setq straight--wait-for-async-jobs t)

;; Tell GCC Emacs in GUI mode where to find libgccjit.
(setenv "LIBRARY_PATH" "/opt/brew/Cellar/libgccjit/11.3.0/lib/gcc/11")

;; Using straight means we don't want to initialize package.el at all.
(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(menu-bar-mode 0)
(tool-bar-mode 0)
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
