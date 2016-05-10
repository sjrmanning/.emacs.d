;; Global custom key bindings.

;; C-w to backward kill word
;; C-x C-k becomes kill-region
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Smarter commenting.
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)

;; M-x alternative (C-x C-m)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Smart beginning of line.
(global-set-key "\C-a" 'smarter-move-beginning-of-line)

;; Joins following line onto current line.
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Global visit ansi-term.
(global-set-key (kbd "<f2>") 'visit-term-buffer)

;; Switch to previous buffer.
(global-set-key (kbd "C-c b") 'switch-to-previous-buffer)

;; Sort-lines.
(global-set-key (kbd "<f5>") 'sort-lines)

;; Full-screen toggle.
(global-set-key "\C-c\M-f" 'toggle-frame-fullscreen)

;; Moving around a bit quicker (5x C-n, C-f etc.) with Shift.
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;; Comment and uncomment region
(define-key global-map (kbd "s-/") 'comment-region)
(define-key global-map (kbd "s-.") 'uncomment-region)

;; Some nicer defaults.
(global-set-key (kbd "C-x k")
                'kill-default-buffer)
(global-set-key (kbd "C-c n")
                'create-new-buffer)
(global-set-key (kbd "C-c N")
                'new-emacs-instance)
(global-set-key (kbd "M-RET")
                'newline-anywhere)
(global-set-key (kbd "C-x C-b")
                'ibuffer)

;; Switch/cycle through ERC buffers.
(global-set-key (kbd "C-c e") 'switch-to-irc)

;; Smart file finding using projectile if available.
(global-set-key (kbd "C-x f") 'smart-find-file)

(provide 'sm-bindings)
