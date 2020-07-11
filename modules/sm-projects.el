;;; sm-projects.el --- Configuration for working with projects.

;; projectile
(use-package projectile
  :delight projectile-mode
  :commands (projectile-mode projectile-global-mode projectile-find-file projectile-project-p)
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
  :config
  (setq persp-initial-frame-name "notes")
  (defun persp-next ()
    (interactive)
    (when (< (+ 1 (persp-curr-position)) (length (persp-all-names)))
      (persp-switch (nth (1+ (persp-curr-position)) (persp-all-names))))))

(provide 'sm-projects)
