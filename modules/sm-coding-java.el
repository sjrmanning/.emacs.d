;;; sm-coding-java.el --- Java and Android config.

(use-package ensime
  :commands java-mode
  :pin melpa-stable)

(use-package java
  :commands java-mode
  :ensure java-imports
  :config
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  :init
  (add-hook 'java-mode-hook
            (lambda ()
              (require 'ensime)
              (ensime-mode t)
              (java-imports-scan-file)
              (bind-key "M-I" 'java-imports-add-import-dwim java-mode-map)
              (setq tab-width 2
                    c-basic-offset 2))))

(provide 'sm-coding-java)
