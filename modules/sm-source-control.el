;;; sm-source-control.el --- Source control and related configuration.

(defun sm/diff-hl-hook ()
  "Trigger normal diff-hl-mode in GUI and margin-mode in terminal."
  (if (display-graphic-p)
      (diff-hl-mode t)
    (progn
      (global-diff-hl-mode t)
      (diff-hl-margin-mode t))))

(use-package diff-hl
  :hook ((prog-mode markdown-mode) . sm/diff-hl-hook)
  :custom
  (diff-hl-draw-borders nil)
  :custom-face
  (diff-hl-change ((t (:background unspecified :foreground "#cda018"))))
  (diff-hl-insert ((t (:background unspecified :foreground "#209e77"))))
  (diff-hl-reverted-hunk-highlight ((t (:background unspecified :foreground "#fb7640"))))
  (diff-hl-delete ((t (:background unspecified :foreground "#c3040f"))))
  :config
  ;; Minimal fringe bitmap.
  (let* ((height (* 1 (frame-char-height)))
       (width 4)
       (ones (1- (expt 2 width)))
       (bits (make-vector height ones)))
  (define-fringe-bitmap 'my-diff-hl-bitmap bits height width))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my-diff-hl-bitmap))
  ;; Update highlighting on the fly.
  (diff-hl-flydiff-mode t))

;; magit
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-diff-refine-hunk 'all))

;; smerge hydra for quicker conflict merging
;; (use-package smerge-mode
;;   :commands smerge-mode
;;   :config
;;   (use-package hydra)
;;   (defhydra sm/smerge-hydra
;;     (:color pink :hint nil :post (smerge-auto-leave))
;;     "
;; ^Move^       ^Keep^               ^Diff^                 ^Other^
;; ^^-----------^^-------------------^^---------------------^^-------
;; _n_ext       _b_ase               _<_: upper/base        _C_ombine
;; _p_rev       _u_pper              _=_: upper/lower       _r_esolve
;; ^^           _l_ower              _>_: base/lower        _k_ill current
;; ^^           _a_ll                _R_efine
;; ^^           _RET_: current       _E_diff
;; "
;;     ("n" smerge-next)
;;     ("p" smerge-prev)
;;     ("b" smerge-keep-base)
;;     ("u" smerge-keep-upper)
;;     ("l" smerge-keep-lower)
;;     ("a" smerge-keep-all)
;;     ("RET" smerge-keep-current)
;;     ("\C-m" smerge-keep-current)
;;     ("<" smerge-diff-base-upper)
;;     ("=" smerge-diff-upper-lower)
;;     (">" smerge-diff-base-lower)
;;     ("R" smerge-refine)
;;     ("E" smerge-ediff)
;;     ("C" smerge-combine-with-next)
;;     ("r" smerge-resolve)
;;     ("k" smerge-kill-current)
;;     ("ZZ" (lambda ()
;;             (interactive)
;;             (save-buffer)
;;             (bury-buffer))
;;      "Save and bury buffer" :color blue)
;;     ("q" nil "cancel" :color blue))
;;   :hook (magit-diff-visit-file . (lambda ()
;;                                    (when smerge-mode
;;                                      (sm/smerge-hydra/body)))))

(provide 'sm-source-control)
