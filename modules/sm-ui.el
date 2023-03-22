;;; sm-ui.el --- UI niceties.

;; Bind for toggling fullscreen.
(use-package frame
  :straight (:type built-in)
  :bind ("C-c M-f" . toggle-frame-fullscreen))

;; uniquify
;; Overrides Emacs' default mechanism for making buffer names unique.
(setq-default uniquify-buffer-name-style 'forward)

(use-package gcmh
  :delight
  :hook emacs-startup
  :commands gcmh-mode
  :functions (gcmh-idle-garbage-collect)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gcmh-verbose nil))

;; Smooth scrolling.
(use-package smooth-scrolling
  :hook after-init)

;; Vertico / orderless / marginalia et al.
(use-package marginalia
  :hook after-init)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :if (display-graphic-p)
  :after marginalia
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
              ("M-DEL" . vertico-directory-delete-word)))

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
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package hotfuzz
  :hook (vertico-mode . hotfuzz-vertico-mode)
  :custom
  (completion-styles '(hotfuzz basic)))

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
  (add-to-list 'consult-buffer-sources persp-consult-source))

(use-package consult-projectile
  :bind ("C-x F" . consult-projectile))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)))

(use-package which-key
  :delight
  :hook after-init
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-width 0.33
        which-key-idle-delay 1.0))

;; diminish some modes.
(use-package simple
  :straight (:type built-in)
  :delight visual-line-mode)

(use-package abbrev
  :straight (:type built-in)
  :delight)

;; ligature support
(use-package ligature
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
