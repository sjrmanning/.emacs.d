;;; sm-coding-general.el --- General coding-related config.

;; EditorConfig.org -- project-local coding style definitions.
(use-package editorconfig
  :commands editorconfig-mode
  :delight editorconfig-mode
  :init (add-hook 'prog-mode-hook #'editorconfig-mode)
  :config
  (progn
    (add-to-list 'editorconfig-indentation-alist
                 '(swift-mode swift-indent-offset))))

;; highlight-numbers
;; Highlights magic numbers in programming modes.
(use-package highlight-numbers
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

;; rainbow-delimiters
;; Highlights parens, brackets, and braces according to their depth.
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; flycheck
(use-package flycheck
  :commands flycheck-mode
  :delight " ✓"
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config (setq flycheck-emacs-lisp-load-path 'inherit))

;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :commands restclient-mode
  :mode ("\\.http$" . restclient-mode))

;; ycmd
;; A code-completion & comprehension server.
(use-package ycmd
  :commands ycmd-mode
  :bind ("C-c y g" . ycmd-goto)
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (unless (or (eq major-mode 'emacs-lisp-mode)
                          (eq major-mode 'lisp-interaction-mode)
                          (eq major-mode 'css-mode)
                          (eq major-mode 'ruby-mode)
                          (eq major-mode 'java-mode)
                          (eq major-mode 'elixir-mode))
                (ycmd-mode))))
  :config
  (setq ycmd-parse-conditions '(save new-line buffer-focus)
        ycmd-idle-change-delay 0.1
        ycmd-server-command '("python" "/usr/local/ycmd/ycmd")
        ycmd-global-config (sm/emacs.d "etc/ycmd_cfg.py")
        ycmd-global-modes '(c++-mode
                            c-mode
                            csharp-mode
                            go-mode
                            js-mode
                            js2-mode
                            objc-mode
                            php-mode
                            python-mode))
  (use-package company-ycmd
    :config
    (add-to-list 'company-backends (sm/backend-with-yas 'company-ycmd)))
  (use-package flycheck-ycmd
    :config (flycheck-ycmd-setup)))

(provide 'sm-coding-general)
