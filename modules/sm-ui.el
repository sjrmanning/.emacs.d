;;; sm-ui.el --- UI niceties.  -*- lexical-binding: t; -*-

;; Show color values like #f9de9f with the actual color as bg.
(use-package rainbow-mode
  :commands rainbow-mode)

;; I don't really use the mouse but it's nice for the scroll wheel
;; and trackpad to work correctly in terminal emacs.
(use-package xt-mouse
  :ensure nil
  :unless (display-graphic-p)
  :hook (elpaca-after-init . xterm-mouse-mode))

(use-package nerd-icons
  :unless (display-graphic-p))

(use-package nerd-icons-completion
  :after marginalia
  :unless (display-graphic-p)
  :ensure (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  :hook elpaca-after-init
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :unless (display-graphic-p)
  :ensure (nerd-icons-dired :type git :host github :repo "rainstormstudio/nerd-icons-dired")
  :hook dired-mode)

(use-package treemacs-nerd-icons
  :unless (display-graphic-p)
  :after treemacs
  :ensure (treemacs-nerd-icons
             :type git :host github :repo "rainstormstudio/treemacs-nerd-icons")
  :init
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))

;; Bind for toggling fullscreen.
(use-package frame
  :if (display-graphic-p)
  :ensure nil
  :bind ("C-c M-f" . toggle-frame-fullscreen))

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(setq-default uniquify-buffer-name-style 'forward)

;; Garbage collection hack.
(use-package gcmh
  :diminish
  :hook emacs-startup
  :commands gcmh-mode
  :functions (gcmh-idle-garbage-collect)
  :custom
  (gcmh-idle-delay 15)
  (gcmh-high-cons-threshold (* 32 1024 1024))
  (gcmh-low-cons-threshold (* 2 1024 1024))
  (gcmh-verbose nil))

;; Better mode-line.
(use-package doom-modeline
  :hook elpaca-after-init
  :custom
  (doom-modeline-height 48))

;; Small improvements to dired.
(use-package dired-x
  :if (display-graphic-p) ;; nerd-fonts in terminal is breaking dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-files "^.DS_Store$\\|^.git$\\|^.projectile$"))

;; https://github.com/kickingvegas/casual-suite
(use-package casual
  :after dired
  :bind (:map dired-mode-map ("C-o" . #'casual-dired-tmenu)))

;; Smooth scrolling.
(use-package smooth-scrolling
  :hook elpaca-after-init)

;; Vertico / orderless / marginalia et al.
(use-package marginalia
  :hook elpaca-after-init)

(use-package all-the-icons
  :if (display-graphic-p)
  :defer t)

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :diminish
  :hook dired-mode
  :custom (all-the-icons-dired-monochrome nil))

(use-package all-the-icons-completion
  :if (display-graphic-p)
  :hook ((elpaca-after-init)
         (marginalia-mode . all-the-icons-completion-marginalia-setup)))

(use-package vertico
  :ensure (vertico :files (:defaults "extensions/*"))
  :hook elpaca-after-init)

(use-package savehist
  :ensure nil
  :after no-littering
  :hook elpaca-after-init
  :custom
  (savehist-file (no-littering-expand-var-file-name "savehist.el"))
  (savehist-autosave-interval nil)
  (savehist-additional-variables
        '(register-alist
          mark-ring global-mark-ring
          search-ring regexp-search-ring)))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-w" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; (use-package hotfuzz
;;   :defer nil
;;   :custom
;;   (completion-styles '(hotfuzz))
;;   ;; (read-buffer-completion-ignore-case t)
;;   )

;; (use-package nucleo-completion
;;   :defer nil
;;   :vc (:url "https://github.com/kn66/nucleo-completion.el" :rev :newest)
;;   :custom
;;   (completion-styles '(nucleo))
;;   (nucleo-completion-sort-ties-by-length t)
;;   )

;; `fzf`
(use-package fzf-native
  :vc (:url "https://github.com/dangduc/fzf-native" :rev :newest)
  :config
  (module-load (expand-file-name "etc/built-modules/fzf-native-module.so" user-emacs-directory)))

(use-package fussy
  :config
  (fussy-setup-fzf)
  (fussy-eglot-setup)
  ;; consult-grep uses load-bearing face text properties to locate
  ;; filename/line boundaries in candidates; fzf-native strips them,
  ;; causing consult--grep-position to crash with wrong-type-argument.
  (add-to-list 'completion-category-overrides '(consult-grep (styles basic))))

;; (use-package fzf-async
;;   :vc (:url "https://github.com/jojojames/fzf-async" :rev :newest)
;;   :bind (("C-c s" . fzf-async-rg)
;;          ("C-x b" . fzf-async-buffer)
;;          ("C-x C-r" . fzf-async-recent-file))
;;          ;; ("C-c b" . consult-project-buffer)
;;          ;; ("M-g" . consult-goto-line))
;;   :config
;;   (fzf-async-setup))

(use-package consult
  :after perspective
  :bind (
         ("C-c s" . consult-ripgrep)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-c b" . consult-project-buffer)
         ("M-g" . consult-goto-line))
  :config
  (setq
   consult--tofu-char #x100000
   consult--tofu-range #x00fffe)
  ;; Delay previews slightly to improve performance.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.1 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  ;; Perspective integration.
  (consult-customize consult-source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  ;; Bookmarks in consult-buffer trigger prog-mode and all its hooks.
  (delete 'consult-source-bookmark consult-buffer-sources))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))

(use-package which-key
  :diminish
  :hook elpaca-after-init
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 1.0))

;; Highlights modified region (e.g. on yank)
(use-package goggles
  :diminish
  :hook (prog-mode text-mode)
  :config
  (setq-default goggles-pulse t))

;; miniframe
;; minibuffer in a childframe like a HUD.
;; (use-package mini-frame
;;   :hook elpaca-after-init
;;   :custom
;;   (mini-frame-ignore-commands '(y-or-n-p
;;                                 eval-expression
;;                                 debugger-eval-expression
;;                                 swiper-isearch
;;                                 swiper-isearch-backward
;;                                 consult-ripgrep
;;                                 consult-goto-line))
;;   (mini-frame-show-parameters '((top . 256)
;;                                 (width . 0.66)
;;                                 (left . 0.5)
;;                                 (child-frame-border-width . 1)
;;                                 (background-color . "#faf5eb"))))

;; ligature support
;; (use-package ligature
;;   :if (display-graphic-p)
;;   :hook prog-mode
;;   :config
;;   (ligature-set-ligatures 'prog-mode
;;                         '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
;;                           ;; =:= =!=
;;                           ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
;;                           ;; ;; ;;;
;;                           (";" (rx (+ ";")))
;;                           ;; && &&&
;;                           ("&" (rx (+ "&")))
;;                           ;; !! !!! !. !: !!. != !== !~
;;                           ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
;;                           ;; ?? ??? ?:  ?=  ?.
;;                           ("?" (rx (or ":" "=" "\." (+ "?"))))
;;                           ;; %% %%%
;;                           ("%" (rx (+ "%")))
;;                           ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
;;                           ;; |->>-||-<<-| |- |== ||=||
;;                           ;; |==>>==<<==<=>==//==/=!==:===>
;;                           ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
;;                                           "-" "=" ))))
;;                           ;; \\ \\\ \/
;;                           ("\\" (rx (or "/" (+ "\\"))))
;;                           ;; ++ +++ ++++ +>
;;                           ("+" (rx (or ">" (+ "+"))))
;;                           ;; :: ::: :::: :> :< := :// ::=
;;                           (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
;;                           ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
;;                           ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
;;                                           "="))))
;;                           ;; .. ... .... .= .- .? ..= ..<
;;                           ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
;;                           ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
;;                           ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
;;                           ;; *> */ *)  ** *** ****
;;                           ("*" (rx (or ">" "/" ")" (+ "*"))))
;;                           ;; www wwww
;;                           ("w" (rx (+ "w")))
;;                           ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
;;                           ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
;;                           ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
;;                           ;; << <<< <<<<
;;                           ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
;;                                           "-"  "/" "|" "="))))
;;                           ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
;;                           ;; >> >>> >>>>
;;                           (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
;;                           ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
;;                           ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
;;                                        (+ "#"))))
;;                           ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
;;                           ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
;;                           ;; __ ___ ____ _|_ __|____|_
;;                          ("_" (rx (+ (or "_" "|"))))
;;                           ;; Fira code: 0xFF 0x12
;;                           ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
;;                           ;; Fira code:
;;                           "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
;;                           ;; The few not covered by the regexps.
;;                           "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^=")))

(provide 'sm-ui)
