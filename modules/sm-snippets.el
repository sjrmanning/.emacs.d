;; yasnippet configuration.  -*- lexical-binding: t; -*-
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :diminish yas-minor-mode
  :custom (yas-snippets-dir (sm/emacs.d "etc/snippets/")))

(provide 'sm-snippets)
