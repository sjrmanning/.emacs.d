;;; sm-coding-java.el --- Java and Android config.

(use-package java
  :ensure java-imports
  :bind ("M-I" . java-imports-add-import-dwim)
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (java-imports-scan-file)
              (setq tab-width 2
                    c-basic-offset 2)))
  :config
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block))

(provide 'sm-coding-java)
