;;; sm-projects.el --- Configuration for working with projects.

;; projectile
(use-package projectile
  :delight projectile-mode
  :commands (projectile-mode projectile-global-mode)
  :bind ;;; ("C-c p r" . projectile-ripgrep)
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map))
  :init
  (add-hook 'after-init-hook 'projectile-global-mode)
  ;; (setq projectile-completion-system 'ivy)
  ;; set to default so that it uses "completing-read" (and selectrum)
  (setq projectile-completion-system 'default)
  :config
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
  :defer t
  :init (add-hook 'after-init-hook 'persp-mode)
  :config
  (setq persp-initial-frame-name "notes")
  (setq persp-suppress-no-prefix-key-warning t)
  (defun persp-next ()
    (interactive)
    (when (< (+ 1 (persp-curr-position)) (length (persp-all-names)))
      (persp-switch (nth (1+ (persp-curr-position)) (persp-all-names))))))

(provide 'sm-projects)
