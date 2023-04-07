;;; sm-projects.el --- Configuration for working with projects.

;; projectile
(use-package projectile
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :hook (after-init . projectile-global-mode)
  :config
  ;; Ensure projectile dir exists.
  (defvar my-projectile-dir (sm/cache-for "projectile"))
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
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-initial-frame-name "notes"))

(provide 'sm-projects)
