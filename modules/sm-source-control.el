;;; sm-source-control.el --- Source control and related configuration.

;; Disable since I use magit for everything.
(setq vc-handled-backends '())
(remove-hook 'find-file-hook 'vc-find-file-hook)

;; gist.el
;; Provides ability to create github gists from region, file, etc., as well as
;; browse, edit, and update metadata of your gists.
;; (use-package gist
;;   :commands gist-region-or-buffer-private
;;   :bind ("C-c g p" . gist-region-or-buffer-private)
;;   :config
;;   (setq gist-view-gist t))

;; magit and monky
;; Modes for git and mercurial.
(use-package magit
  :commands magit-status
  :hook (git-commit-mode . flyspell-mode)
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-diff-refine-hunk 'all))

;; Git forge w/ magit.
(use-package forge
  :after magit)

;; smerge hydra for quicker confluct merging!
(use-package hydra)
(use-package smerge-mode
  :commands smerge-mode
  :requires hydra
  :after hydra
  :config
  (defhydra sm/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (sm/smerge-hydra/body)))))

(provide 'sm-source-control)
