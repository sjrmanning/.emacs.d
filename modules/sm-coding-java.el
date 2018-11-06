;;; sm-coding-java.el --- Java and Android config.

(use-package ensime
  :straight (:host github :repo "ensime/ensime-emacs" :branch "2.0")
  :commands java-mode)

(use-package java
  :straight java-imports
  :commands java-mode
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
