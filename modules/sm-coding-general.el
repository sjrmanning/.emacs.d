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
  :straight (:host github :repo "flycheck/flycheck"
                   :commit "cd8e0a280c9980c8c7ce31fd2458df7fd81a0acf")
  :commands flycheck-mode
  :delight " ✓"
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config (setq flycheck-emacs-lisp-load-path 'inherit))

;; restclient
;; Runs REST queries from a query sheet and pretty-prints responses.
(use-package restclient
  :commands restclient-mode
  :mode ("\\.http$" . restclient-mode))

;; See
;;  https://utcc.utoronto.ca/~cks/space/blog/programming/GoEmacsWithLspMode
;;  https://ladicle.com/post/config/#lsp
(use-package lsp-mode
  :straight `(lsp-mode :repo "emacs-lsp/lsp-mode"
                       :host github
                       :files (:defaults
                               "clients/*.el"))
  :commands (lsp lsp-deferred)
  ;; (setq lsp-ui-sideline-show-code-actions nil)
  :custom
  ;; (lsp-auto-guess-root t)        ;; requires projectile/project?
  ;; sync methods: none, full, incremental, or nil (nil == use lsp suggestion)
  (lsp-document-sync-method nil)
  (lsp-prefer-flymake nil) ;; t(flymake), nil(lsp-ui/flycheck), or :none
  (lsp-pylsp-server-command "/compbio/home/george.hartzell/tmp/lsp-venv/bin/pylsp")
  :hook ((go-mode . lsp-deferred)
         (python-mode . lsp-deferred))
  :bind
  (:map lsp-mode-map
        ("C-c r" . lsp-rename))
  :config
  ;; optional - provides fancier overlays
  (use-package lsp-ui
    :commands lsp-ui-mode
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-delay 1)
    (lsp-ui-sideline-delay 2)
    (lsp-ui-sideline-show-code-actions nil)
    (lsp-ui-sideline-show-hover nil)

    ;; lsp-ui-doc
    (lsp-ui-doc-enable nil)
    (lsp-ui-doc-header t)
    (lsp-ui-doc-include-signature nil)
    (lsp-ui-doc-position 'at-point) ;; top, bottom, or at-point
    (lsp-ui-doc-max-width 120)
    (lsp-ui-doc-max-height 30)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-use-webkit t)
    ;; lsp-ui-flycheck
    ;; sjrmanning sets this to t in his....
    (lsp-ui-flycheck-enable nil)
    ;; lsp-ui-sideline
    (lsp-ui-sideline-enable nil)
    (lsp-ui-sideline-ignore-duplicate t)
    ;; (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-sideline-show-diagnostics nil)
    ;; (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-code-actions-prefix "")
    ;; lsp-ui-imenu
    (lsp-ui-imenu-enable t)
    (lsp-ui-imenu-kind-position 'top)
    ;; lsp-ui-peek
    (lsp-ui-peek-enable t)
    (lsp-ui-peek-peek-height 20)
    (lsp-ui-peek-list-width 50)
    (lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
    :preface
    (defun ladicle/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
          (progn
            (lsp-ui-doc-mode -1)
            (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))
    :bind
    (:map lsp-mode-map
          ("C-c C-r" . lsp-ui-peek-find-references)
          ("C-c C-j" . lsp-ui-peek-find-definitions)
          ("C-c i"   . lsp-ui-peek-find-implementation)
          ("C-c m"   . lsp-ui-imenu)
          ("C-c s"   . lsp-ui-sideline-mode)
          ("C-c d"   . ladicle/toggle-lsp-ui-doc))
    )
  ;; (use-package lsp-origami
  ;;   :config
  ;;   (origami-mode 1)
  ;;   :hook (lsp-mode . lsp-origami-mode))
  )

;; if you use company-mode for completion (otherwise,
;; complete-at-point works out of the box):
;; (use-package company-lsp
;;   :commands company-lsp)

(use-package origami
  :hook (go-mode . origami-mode)
  )

(use-package dockerfile-mode)

(provide 'sm-coding-general)
