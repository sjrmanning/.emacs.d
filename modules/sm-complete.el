;;; sm-complete.el --- Auto-completion.

;;; Commentary:

;; This file configures auto-completion niceties.

;;; Code:

(use-package corfu
  :hook ((lsp-completion-mode . #'sm/corfu-setup-lsp)
         (prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (org-mode . corfu-mode))
  :bind (:map corfu-map
	            ("C-n" . #'corfu-next)
              ("C-p" . #'corfu-previous)
              ("<escape>" . #'corfu-quit)
	            ("<return>" . #'corfu-insert)
	            ("M-d" . #'corfu-show-documentation)
              ("M-l" . #'corfu-show-location))
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  (corfu-quit-at-boundary nil)
  (corfu-echo-documentation nil)
  (lsp-completion-provider :none)

  :config
  (defun sm/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf instead of the
default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/"))
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
	            ([remap corfu-show-documentation] . #'corfu-doc-toggle)
	            ("M-n" . #'corfu-doc-scroll-up)
              ("M-p" . #'corfu-doc-scroll-down))
  :custom
  (corfu-doc-delay 0.5)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20)
  (corfu-echo-documentation nil))

(use-package corfu-terminal
  :unless window-system
  :after corfu
  :hook (corfu-mode . corfu-terminal-mode))

(use-package corfu-doc-terminal
  :unless window-system
  :straight
  (:type git
	       :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
  :after corfu
  :hook (corfu-mode . corfu-doc-terminal-mode))

;;; _
(provide 'sm-complete)
;;; sm-complete.el ends here
