;; yasnippet configuration.
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :delight yas-minor-mode
  :config
  ;; Ensure custom snippets dir exists.
  (defvar custom-snippets-dir (sm/emacs.d "etc/snippets/"))
  (sm/mkdir-p custom-snippets-dir)
  ;; Replace default custom dir with our own.
  (setq yas-snippet-dirs '(custom-snippets-dir
                           yas-installed-snippets-dir))
  ;; Suppress excessive log messages
  (setq yas-verbosity 1)
  :config
  ;; Ensure that this dir exists, avoid warning.
  (sm/mkdir-p yas-installed-snippets-dir)
  (yas-global-mode t)
  ;; Disable yasnippet in some modes.
  (defun yas-disable-hook ()
    (setq yas-dont-activate t))
  (add-hook 'term-mode-hook #'yas-disable-hook)
  (add-hook 'comint-mode-hook #'yas-disable-hook)
  (add-hook 'erc-mode-hook #'yas-disable-hook))

(provide 'sm-snippets)
