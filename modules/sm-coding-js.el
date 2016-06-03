;; sm-coding-js.el --- JavaScript and json configuration.

(use-package javascript-mode
  :ensure nil
  :mode ("\\.json$" . javascript-mode)
  :init
  (add-hook 'js-mode-hook (lambda () (setq js-indent-level 4))))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
         ("Jakefile$" . js2-mode))
  :interpreter "node"
  :config
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-basic-offset 2))))

(provide 'sm-coding-js)
