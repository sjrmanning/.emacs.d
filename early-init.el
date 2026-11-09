;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(setq my-cpu-architecture "apple-m1")

(setq native-comp-compiler-options `(;; The most meaningful optimizations:
                                     ;; "-O2"
;;                                      ,(format "-mtune=%s" my-cpu-architecture)
                                     ,(format "-mcpu=%s" my-cpu-architecture)
;;                                      ;; Reduce .eln size and compilation
;;                                      ;; overhead.
                                     ;; "-g0"
;;                                      ;; Good defensive choice for Emacs
;;                                      ;; stability.
                                     ;; "-fno-omit-frame-pointer"
                                     ;; "-fno-finite-math-only"
				     ))

;; (setq native-comp-driver-options '(;; -Wl,-z,pack-relative-relocs compresses
;;                                    ;; relocation tables to reduce file size and
;;                                    ;; slightly improve load times.
;;                                    ;; "-Wl,-z,pack-relative-relocs"
;;                                    ;; -Wl,-O2 applies standard linker-level
;;                                    ;; optimizations (like string merging) to the
;;                                    ;; generated shared object.
;;                                    "-Wl,-O2"
;;                                    ;; -Wl,--as-needed prevents the linker from
;;                                    ;; recording dependencies on libraries that
;;                                    ;; are not actually used by the code.
;;                                    ;; "-Wl,--as-needed"
;; 				   ))

;; Remove searching for .gz files.
(setq jka-compr-load-suffixes nil)

;; Set file-name-handler-alist to nil, to avoid a lot of regexp searches.
;; Restored in emacs-startup-hook (init.el).
(setq sm/file-name-handler-alist-orig file-name-handler-alist)
(setq file-name-handler-alist nil)

;; GCC Emacs optimization setting.
(setq-default native-comp-speed 3)

;; Using straight means we don't want to initialize package.el at all.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Allow Emacs to resize per-pixel, not per character width.
(setq-default frame-resize-pixelwise t
              window-resize-pixelwise t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; Replace startup message with init-time.
(fset 'display-startup-echo-area-message
      (lambda () (message "Loaded in %s with %d GCs (%f elapsed)."
                          (emacs-init-time) gcs-done gc-elapsed)))

;; Don't show buffer menu when opening multiple files.
(setq inhibit-startup-buffer-menu t)
