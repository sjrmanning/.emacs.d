;; yasnippet configuration.
(use-package yasnippet
  :defer 2
  :init
  ;; Ensure custom snippets dir exists.
  (defvar custom-snippets-dir (sm/emacs.d "etc/snippets/"))
  (sm/mkdir-p custom-snippets-dir)
  ;; Replace default custom dir with our own.
  (setq yas-snippet-dirs '(custom-snippets-dir
                           yas-installed-snippets-dir))
  ;; Suppress excessive log messages
  (setq yas-verbosity 1)
  :config
  (yas-global-mode t)
  ;; Disable yasnippet in some modes.
  (defun yas-disable-hook ()
    (setq yas-dont-activate t))
  (add-hook 'term-mode-hook #'yas-disable-hook)
  (add-hook 'comint-mode-hook #'yas-disable-hook)
  (add-hook 'erc-mode-hook #'yas-disable-hook))

(provide 'sm-snippets)
