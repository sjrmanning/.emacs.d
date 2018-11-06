;;; sm-source-control.el --- Source control and related configuration.

;; Disable since I use magit for everything.
(setq vc-handled-backends '())
(remove-hook 'find-file-hooks 'vc-find-file-hook)

;; gist.el
;; Provides ability to create github gists from region, file, etc., as well as
;; browse, edit, and update metadata of your gists.
;; (use-package gist
;;   :commands gist-region-or-buffer-private
;;   :bind ("C-c g p" . gist-region-or-buffer-private)
;;   :config
;;   (setq gist-view-gist t))

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
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (add-hook 'git-commit-mode-hook '(lambda () (flyspell-mode t))))

;; git-gutter
(use-package git-gutter
  :straight git-gutter-fringe
  :defer 2
  :delight git-gutter-mode
  :config
  (require 'git-gutter-fringe)
  (setq git-gutter:handled-backends '(git hg))
  (global-git-gutter-mode t))

(provide 'sm-source-control)
