;;; sm-projects.el --- Configuration for working with projects.

;; projectile
(use-package projectile
  :delight projectile-mode
  :commands (projectile-mode projectile-global-mode projectile-find-file projectile-project-p projectile-project-root)
  :custom (shell-file-name "/bin/bash")
  :config
  (setq projectile-completion-system 'default)
  ;; Ensure projectile dir exists.
  (defvar my-projectile-dir (sm/cache-for "projectile"))
  (sm/mkdir-p my-projectile-dir)
  ;; Use projectile dir for cache and bookmarks.
  (let* ((prj-dir (file-name-as-directory my-projectile-dir))
         (prj-cache-file (concat prj-dir "projectile.cache"))
         (prj-bookmarks-file (concat prj-dir "projectile-bkmrks.eld")))
    (setq projectile-cache-file          prj-cache-file
          projectile-known-projects-file prj-bookmarks-file
          projectile-indexing-method     'alien)))

;; perspective
(use-package perspective
  :hook (after-init . persp-mode)
  :bind ("C-x b" . persp-switch-to-buffer*)
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-initial-frame-name "notes"))

(provide 'sm-projects)
