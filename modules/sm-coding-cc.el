;;; sm-coding-cc.el --- cc-mode and derived modes config.

;; Parent mode to C/C++/ObjC/Java.
(use-package cc-mode
  :defer t
  :config
  (defun sm/cc-mode-hook ()
    (c-set-offset 'case-label '+)
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'inexpr-class 0))
  (add-hook 'c-mode-common-hook 'sm/cc-mode-hook)

  ;; Objective-C
  (add-hook 'objc-mode-hook
            '(lambda ()
               (setq c-basic-offset 4))))

(use-package irony
  :commands irony-mode
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  (use-package flycheck-irony)
  (use-package company-irony
    :config
    (add-to-list 'company-backends 'company-irony))
  (setq irony-server-install-prefix (sm/cache-for "irony"))
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (flycheck-irony-setup))

;; dummy-h-mode
;; Determines c/c++/objc mode based on contents of a .h file.
(use-package dummy-h-mode
  :mode "\\.h$")

(provide 'sm-coding-cc)
