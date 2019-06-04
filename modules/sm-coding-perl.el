;;; sm-coding-perl.el --- Perl config.

;; perl mode setup.
;; Use cperl mode instead of the default perl mode
(use-package cperl-mode
  :init
  (progn
    (defalias 'perl-mode 'cperl-mode)
    (add-hook 'cperl-mode-hook
              '(lambda ()
                 ;; insert spaces instead of tabs
                 (setq-default indent-tabs-mode nil)

                 ;; set line width to 78 columns
                 (setq fill-column 78)
                 (setq auto-fill-mode t)

                 ;; something that's close to the perltidy defaults
                 (cperl-set-style "BSD")
                 (setq cperl-indent-parens-as-block t
                       cperl-close-paren-offset -4)
                 ))
    ))

;; tidyall support
(use-package tidyall
  :straight nil
  :load-path "etc/extra"
  :config
  :init
  (progn
    (add-hook 'cperl-mode-hook
              (progn
                (lambda () (add-hook 'before-save-hook #'tidyall-buffer nil 'local)))))
  :bind (:map cperl-mode-map
              ("\C-x t". tidyall-buffer)))

(provide 'sm-coding-perl)
