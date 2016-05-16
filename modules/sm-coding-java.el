;;; sm-coding-java.el --- Java and Android config.

(use-package java
  :commands java-mode
  :ensure java-imports
  :ensure ggtags
  :init
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  (add-hook 'java-mode-hook
            (lambda ()
              (ggtags-mode)
              (java-imports-scan-file)
              (bind-key "M-I" 'java-imports-add-import-dwim java-mode-map)
              (setq tab-width 2
                    c-basic-offset 2))))

(provide 'sm-coding-java)
