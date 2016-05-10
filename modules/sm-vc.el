;;; sm-vc.el --- Version control config.

;; magit and monky
;; Modes for git and mercurial.
(use-package magit
  :commands magit-status
  :bind ("C-x g" . magit-or-monky-status)
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  ;; Full-screen magit status with restore.
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-mode-quit-window (around magit-restore-screen activate)
    ad-do-it
    (jump-to-register :magit-fullscreen))
  ;; Use flyspell during commits.
  (add-hook 'git-commit-mode-hook '(lambda () (flyspell-mode t))))

(use-package monky
  :commands monky-status
  :config
  ;; Similar full-screen config for monky.
  (defadvice monky-status (around monky-fullscreen activate)
    (window-configuration-to-register :monky-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice monky-quit-window (around monky-restore-screen activate)
    ad-do-it
    (jump-to-register :monky-fullscreen))
  ;; Flyspell during commits.
  (add-hook 'monky-log-edit-mode-hook '(lambda () (flyspell-mode t))))

;; git-gutter
(use-package git-gutter
  :defer 5
  :diminish git-gutter-mode
  :ensure git-gutter-fringe
  :config
  (require 'git-gutter-fringe)
  (setq git-gutter:handled-backends '(git hg))
  (global-git-gutter-mode t))

(provide 'sm-vc)
