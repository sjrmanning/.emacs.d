;;; sm-keybindings.el --- keybindings, simple and/or icky black magic

;;; Code:

;; Global keychords, not associated with any particular package.
(key-chord-define-global "xx" 'execute-extended-command)

;;
;; So far I've set up:
;; C-. to Esc [46;5u
;; C-< to Esc [60;6u
;; C-= to Esc [61;5u
;; C-> to Esc [62;6u
;;
;; See https://emacs.stackexchange.com/questions/1020/problems-with-keybindings-when-using-terminal/13957#13957
;; There seem to be two competing standards for mapping key strokes in
;; dump terminals into escape sequences
;;
;; One is LeoNerd's libtermkey with the syntax: `ESC [ codepoint ;
;; modifier u` [details
;; here](http://www.leonerd.org.uk/hacks/fixterms/)
;; Another is Thomas Dickey's xterm with the syntax `ESC [ 2 7 ; modifier ; codepoint ~`
;;
;; I've chosen to follow LeoNerd's standard (it's shorter, for lack of a better reason).
;;
;; In iTerm2, go into the preferences, keys, key mappings section and
;; add a new mapping.  E.g. to add a mapping for "C-<":
;; In the keyboard shortcut section,
;; 1. click to set the Keyboard Shortcut
;;    - then hit the control key (which in my case has been swapped with
;;      capslock at the OS X level),
;;    - then the shift,
;;    - then the <.
;;    In the shortcut box you'll see ^ then an up arrow, then a
;;    *comma*.  Don't Panic....
;; 2. For an Action, choose 'Send Escape Sequence'
;; 3. Send Esc + [60;6u
;;
;; When you're done, you'll notice that in the Key Combination dialog
;; it now says "^ up arrow <" (even though is used a ',' instead of
;; a '<' while you were entering it.  Go figure...)
;;
;; Note that even though the '<' is on a shifted key, you need to use
;; '6' instead of '5' (include the shift bit...)
;;
;; Now add a key binding for that key, e.g. (if you're using use-package):
;;  :bind ((("C-<" . mc/mark-previous-like-this)
;;          ("C->" . mc/mark-next-like-this))
;;  ...
;;
;; Restart emacs and try M-x describe-key C-< and you should see the
;; info for mc/mark-previous-like-this.
;;
;; What follows below is from Gilles answer, modified to end sequences
;; with a 'u' instead of a '~' in the second part of the loop (Gilles
;; had a bug?).
;;
;; xterm with the resource ?.VT100.modifyOtherKeys: 1
;; GNU Emacs >=24.4 sets xterm in this mode and define
;; some of the escape sequences but not all of them.
(defun character-apply-modifiers (c &rest modifiers)
  "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
  (if (memq 'control modifiers) (setq c (if (or (and (<= ?@ c) (<= c ?_))
                                                (and (<= ?a c) (<= c ?z)))
                                            (logand c ?\x1f)
                                          (logior (lsh 1 26) c))))
  (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
  (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
  (vector c))
(defun my-eval-after-load-xterm ()
  (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
    (let ((c 32))
      (while (<= c 126)
        (mapc (lambda (x)
                (define-key xterm-function-map (format (car x) c)
                  (apply 'character-apply-modifiers c (cdr x))))
              '(;; with ?.VT100.formatOtherKeys: 0
                ("\e\[27;3;%d~" meta)
                ("\e\[27;5;%d~" control)
                ("\e\[27;6;%d~" control shift)
                ("\e\[27;7;%d~" control meta)
                ("\e\[27;8;%d~" control meta shift)
                ;; with ?.VT100.formatOtherKeys: 1
                ("\e\[%d;3u" meta)
                ("\e\[%d;5u" control)
                ("\e\[%d;6u" control shift)
                ("\e\[%d;7u" control meta)
                ("\e\[%d;8u" control meta shift)))
        (setq c (1+ c))))))
(eval-after-load "xterm" '(my-eval-after-load-xterm))

(provide 'sm-keybindings)
