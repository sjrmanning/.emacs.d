;;; sm-vc.el --- Version control config.

(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(defun sm/magit-or-monky-status ()
  "Call `magit-status' or `monky-status' depending on whether a
git or hg repository is found in the buffer-local working dir."
  (interactive)
  (cond
   ((eq (car (process-exit-code-and-output "hg" "status")) 0)
    (monky-status))
   ((eq (car (process-exit-code-and-output "git" "status")) 0)
    (call-interactively 'magit-status))
   (t (message "No hg or git repository found at %s" default-directory))))

;; magit and monky
;; Modes for git and mercurial.
(use-package magit
  :commands magit-status
  :bind ("C-x g" . sm/magit-or-monky-status)
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
