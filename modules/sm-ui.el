;;; sm-ui.el --- UI niceties.

;; Bind for toggling fullscreen.
(use-package frame
  :if (display-graphic-p)
  :straight (:type built-in)
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
  (gcmh-idle-delay 'auto)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-verbose nil))

;; Better mode-line.
(use-package doom-modeline
  :hook after-init)

;; Smooth scrolling.
(use-package smooth-scrolling
  :hook after-init)

;; Vertico / orderless / marginalia et al.
(use-package marginalia
  :hook after-init)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :diminish
  :hook dired-mode
  :custom (all-the-icons-dired-monochrome nil))

(use-package all-the-icons-completion
  :if (display-graphic-p)
  :hook ((after-init)
         (marginalia-mode . all-the-icons-completion-marginalia-setup)))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (;; vertico-indexed
                                ;; vertico-flat
                                ;; vertico-grid
                                ;; vertico-mouse
                                ;; vertico-quick
                                ;; vertico-buffer
                                ;; vertico-repeat
                                ;; vertico-reverse
                                vertico-directory
                                ;; vertico-multiform
                                ;; vertico-unobtrusive
                                ))
  :hook after-init
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-w" . vertico-directory-delete-word)))

(use-package savehist
  :hook after-init
  :custom
  (savehist-file (no-littering-expand-var-file-name "savehist.el"))
  (savehist-autosave-interval nil)
  (savehist-additional-variables
        '(register-alist
          mark-ring global-mark-ring
          search-ring regexp-search-ring)))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-w" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :commands (orderless-filter))

(use-package fussy
  :config
  (with-eval-after-load 'corfu
    (advice-add 'corfu--capf-wrapper :before 'fussy-wipe-cache))
  (with-eval-after-load 'eglot
    (add-to-list 'completion-category-overrides
                 '(eglot (styles fussy basic))))
  :custom
  (fussy-use-cache t)
  ;; (fussy-filter-fn 'fussy-filter-orderless)
  (fussy-filter-fn 'fussy-filter-default)
  (completion-styles '(fussy basic))
  (completion-category-defaults nil)
  (completion-category-overrides nil))

(use-package fuz-bin
  :defer nil
  :straight
  (fuz-bin
   :repo "jcs-elpa/fuz-bin"
   :fetcher github
   :files (:defaults "bin"))
  :config
  (setq fussy-score-fn 'fussy-fuz-bin-score)
  (fuz-bin-load-dyn t))

(use-package consult
  :after perspective
  :bind (("C-c s" . consult-ripgrep)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-c b" . consult-project-buffer)
         ("M-g" . consult-goto-line))
  :config
  ;; Delay previews slightly to improve performance.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  ;; Perspective integration.
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  ;; Bookmarks in consult-buffer trigger prog-mode and all its hooks.
  (delete 'consult--source-bookmark consult-buffer-sources))

(use-package consult-projectile
  :bind ("C-x F" . consult-projectile))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))

(use-package which-key
  :diminish
  :hook after-init
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-side-window-max-width 0.33)
  (which-key-idle-delay 1.0))

(use-package treemacs
  :custom
  (treemacs-resize-icons 44)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x C-n"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :defer nil)

(use-package treemacs-magit
  :after (treemacs magit)
  :defer nil)

;; Highlights modified region (e.g. on yank)
;; Weird behavior on terminal so only in GUI for now.
(use-package goggles
  :if (display-graphic-p)
  :diminish
  :hook (prog-mode text-mode)
  :config
  (setq-default goggles-pulse t))

;; ligature support
(use-package ligature
  :if (display-graphic-p)
  :hook prog-mode
  :config
  (ligature-set-ligatures 'prog-mode
                        '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                          ;; =:= =!=
                          ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                          ;; ;; ;;;
                          (";" (rx (+ ";")))
                          ;; && &&&
                          ("&" (rx (+ "&")))
                          ;; !! !!! !. !: !!. != !== !~
                          ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                          ;; ?? ??? ?:  ?=  ?.
                          ("?" (rx (or ":" "=" "\." (+ "?"))))
                          ;; %% %%%
                          ("%" (rx (+ "%")))
                          ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                          ;; |->>-||-<<-| |- |== ||=||
                          ;; |==>>==<<==<=>==//==/=!==:===>
                          ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                          "-" "=" ))))
                          ;; \\ \\\ \/
                          ("\\" (rx (or "/" (+ "\\"))))
                          ;; ++ +++ ++++ +>
                          ("+" (rx (or ">" (+ "+"))))
                          ;; :: ::: :::: :> :< := :// ::=
                          (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                          ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                          ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                          "="))))
                          ;; .. ... .... .= .- .? ..= ..<
                          ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                          ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                          ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                          ;; *> */ *)  ** *** ****
                          ("*" (rx (or ">" "/" ")" (+ "*"))))
                          ;; www wwww
                          ("w" (rx (+ "w")))
                          ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                          ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                          ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                          ;; << <<< <<<<
                          ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                          "-"  "/" "|" "="))))
                          ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                          ;; >> >>> >>>>
                          (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                          ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                          ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                       (+ "#"))))
                          ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                          ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                          ;; __ ___ ____ _|_ __|____|_
                          ("_" (rx (+ (or "_" "|"))))
                          ;; Fira code: 0xFF 0x12
                          ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                          ;; Fira code:
                          "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                          ;; The few not covered by the regexps.
                          "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^=")))

(provide 'sm-ui)
