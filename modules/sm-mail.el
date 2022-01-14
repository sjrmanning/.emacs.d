;;; sm-mail.el --- VM and BBDB and oh my!

;; don't bother with this, except on my laptop...
;; (when (string= (system-name) "alec.local")
(when (string-match "^alec" (system-name))
  (use-package ebdb
    :straight (ebdb :type git :host github :repo "girzel/ebdb"
                    )
    :config
    (setq ebdb-case-fold-search t)
    )
  (use-package bbdb
    :commands (bbdb bbdb-insinuate-vm)
    :init
    (require 's)
    (load "bbdb-autoloads" nil t)
    :config
    (setq bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook)
    (setq bbdb-completion-type 'primary-or-name)
    (setq bbdb-allow-duplicates t)
    (setq bbdb/vm-update-records-mode 'searching)
    ;; cons and add these step-wise for expansion's sake
    ;; (I can't figure out how to s-join the To list otherwise)
    (progn
      (setq bbdb-ignore-some-messages-alist ())
      (add-to-list 'bbdb-ignore-some-messages-alist
                   (cons "From"
                         (s-join "\\|"
                                 (list
                                  "rockridgeca.nextdoor.com"
                                  "yahoogroups.com"
                                  "reply.github.com"
                                  "github.com"
                                  ".*nextdoor.com"
                                  ))))
      (add-to-list 'bbdb-ignore-some-messages-alist
                   (cons "Reply-To" "reply.github.com"))
      (add-to-list 'bbdb-ignore-some-messages-alist
                   (cons "To"
                         (s-join "\\|"
                                 (list
                                  "rockridgeca.nextdoor.com"
                                  "googlegroups.com"
                                  "freebsd.org"
                                  "yahoogroups.com"
                                  "noreply.github.com"
                                  "reply.github.com"
                                  "github.com"
                                  "terraform@noreply.github.com"
                                  "slimserver@noreply.github.com"
                                  "notifications@github.com"
                                  ".*nextdoor.com"
                                  )))))
    (setq bbdb-quiet-about-name-mismatches 4) ; warn for 4 secs
    (setq bbdb-use-pop-up nil)
    )

  ;; (add-to-list 'load-path (sm/emacs.d "etc/extra/vm-8.2.0b"))
  ;; (add-to-list 'load-path (sm/emacs.d "etc/extra/vm-8.2.0b/lisp"))
  ;; (add-to-list 'load-path "/Users/hartzell/tmp/vm")
  ;; (add-to-list 'load-path "/Users/hartzell/tmp/vm/lisp")
  ;; (load (sm/emacs.d "etc/extra/my-setup-mail.el"))

  (defun my-vm-build (package &rest _)
    (when (string= package "vm")
      (let ((default-directory (straight--repos-dir "vm")))
        (straight--get-call "autoconf")
        (straight--get-call (concat default-directory "/configure")
                            (concat "--with-other-dirs=" (straight--build-dir "bbdb")))
        (straight--get-call "make")))
    )
  (add-hook 'straight-use-package-pre-build-functions #'my-vm-build)

  (use-package vm
    :straight (vm :type git :host github :repo "emacsmirror/vm"
                  :fork (:host github
                               :repo "hartzell/vm"
                               :branch "oldie"
                               ;; :branch "wip"
                               )
                  :no-byte-compile t
                  :files (:defaults "lisp/*.elc")
                  )
    :commands (vm)

    :init
    (require 'cl)
    (require 'vm-pgg)
    :config
    ;; Make VM your default mail agent in Emacs
    (setq mail-user-agent 'vm-user-agent)


    (setq vm-berkeley-mail-compatibility t)
    (setq vm-circular-folders nil)
    (setq vm-complete-mail-allow-cycling t)
    (setq vm-delete-empty-folders t)
    (setq vm-digest-burst-type "mime")
    ;; make mime first in the list, so I can just bang the return key
    (setq vm-digest-type-alist '(("mime") ("rfc934") ("rfc1153")))
    (setq vm-folder-directory "~/mail/")
    (setq vm-highlighted-header-regexp "^\From\\|^Subject")
    (setq vm-move-after-deleting t)
    (setq vm-preview-lines nil)
    (setq vm-startup-with-summary 1)
    (setq vm-summary-format "%n %*%a %-17.17F %-3.3m %2d %5l/%-7c %I\"%s\"\n")
    (setq vm-reply-subject-prefix "Re: ")
    (setq vm-visit-when-saving t)
    (setq vm-window-configuration-file "~/.vm.windows")
    (setq vm-jump-to-unread-messages nil)
    (setq vm-jump-to-new-messages nil)
    (setq vm-auto-get-new-mail nil)
    (setq vm-mail-check-interval nil)
    (setq vm-mime-use-image-strips nil)
    (setq vm-frame-parameter-alist '((folder ( (width . 130) (height . 45)))))
    (setq vm-mime-text/html-handler 'auto-select)
    (setq vm-use-toolbar nil)

    ;; use IMAP instead... (setq vm-primary-inbox "~/mail/inbox")
    ;; (setq vm-primary-inbox "imap-ssl:griffon.alerce.com:23:INBOX:login:hartzell:*")
    (setq vm-primary-inbox "imap-ssl:corvid.alerce.com:993:inbox:login:hartzell:*")
    (setq vm-spool-files
          '(
            "/var/mail/hartzell"
            ;; special imap3s listener running on telnet port for access from genentech.
            ;; "imap-ssl:griffon.alerce.com:23:inbox:login:hartzell:*"
            ;; special pop3s listener running on telnet port for access from genentech.
            ;; "pop-ssl:griffon.alerce.com:23:pass:hartzell:*"
            ;; hit the pop3s server on the normal port
            ;; "pop-ssl:griffon.alerce.com:995:pass:hartzell:*"
            ))
    (setq vm-imap-account-alist
          ;; for other IMAP servers
          '(
            ;; ("imap-ssl:imap.gmail.com:993:inbox:login:georgewh@gene.com:*" "gne")
            ;; ("imap-ssl:corvid.alerce.com:993:inbox:login:hartzell:*" "hartzell@corvid.alerce.com")
            ("imap-ssl:corvid.alerce.com:993:inbox:login:hartzell:*" "hartzell@alerce.com")
            ("imap-ssl:corvid.alerce.com:993:inbox:login:hartzell@georgehartzell.com:*" "georgehartzell.com-imap")
            ("imap-ssl:corvid.alerce.com:993:inbox:login:hartzell@baulines.com:*" "baulines.com-imap")
            ))
    (setq vm-imap-auto-expunge-alist
          '(
            ("imap-ssl:griffon.alerce.com:993:inbox:login:hartzell:*" . t)
            ("imap-ssl:corvid.alerce.com:993:inbox:login:hartzell:*" . t)
            ))

    (add-hook 'vm-mode-hook
              (lambda ()
                (setq vm-visible-headers
                      (append vm-visible-headers (list "X-Spam-Status:")))
                ;; (bbdb-insinuate-vm)
                (bbdb-initialize 'vm)
                (setq bbdb-use-pop-up nil)
                ;; (setq vm-auto-decode-mime-messages nil)
                (setq vm-save-killed-message nil)
                (setq vm-auto-displayed-mime-content-type-exceptions
                      '("text/html"))
                (define-key vm-mode-map "$" 'vm-save-message-to-imap-folder)
                (define-key vm-mode-map "\C-c\C-o" 'vm-send-message-to-omnifocus)
                (vm-v7-key-bindings)
                ;; add pile of charsets to vm-mime-default-face-charsets
                (let ((charsets '("iso-8859-1" "iso8859-1" "iso-8859-5"
                                  "iso-8859-6" "iso-8859-2" "iso-8859-7"
                                  "iso-8859-9" "iso8859-15" "ISO-8859-15"
                                  "windows-1250" "windows-1251" "Windows-1252"
                                  "koi8-r" "KOI8-U" "utf-8" "big5" "tis-620"
                                  "gb2312" "iso-2022-jp" "windows-1256"
                                  "X-UNKNOWN" "euc-kr" "ISO8859-2")))
                  (while charsets
                    (add-to-list 'vm-mime-default-face-charsets (pop charsets))))
                (set-variable 'vm-mime-alternative-show-method
                              '(favorite "text/plain"))
                (set-variable 'vm-mime-alternative-yank-method
                              '(favorite "text/plain"))
                ))
    (setq mail-default-reply-to "hartzell@alerce.com")
    (setq user-mail-address "hartzell@alerce.com")

    (add-hook 'mail-mode-hook
              (lambda ()
                ;;define mail-archive-file-name to ~/mail/archive/<year-month>
                (let ((date (current-time-string)))
                  (string-match
                   "^\\([A-Z][a-z][a-z]\\) \\([A-Z][a-z][a-z]\\) \\([0-9 ][0-9]\\) \\([0-9][0-9]:[0-9][0-9]\\)\\(:[0-9][0-9]\\) [0-9][0-9]\\([0-9][0-9]\\)"
                   date)
                  (setq mail-archive-file-name
                        (concat vm-folder-directory
                                "Archive/"
                                (substring date (match-beginning 6) (match-end 6))
                                "-"
                                (substring date (match-beginning 2) (match-end 2))
                                )))
                ;;set other defaults
                (define-key mail-mode-map "\e\^i" 'bbdb-complete-name)
                (setq mail-signature nil)
                (auto-fill-mode 1)))      ;turn on auto-fill

    (setq send-mail-function 'sendmail-send-it)
    (setq message-send-mail-function 'sendmail-send-it)
    ;;(setq send-mail-function 'smtpmail-send-it)
    ;;(setq message-send-mail-function 'smtpmail-send-it)
    ;;(setq smtpmail-smtp-server "localhost")

    (setq mail-setup-with-from t)

    (require 'u-vm-color)
    (add-hook 'vm-summary-mode-hook 'u-vm-color-summary-mode)
    (add-hook 'vm-select-message-hook 'u-vm-color-fontify-buffer)
    ;;  It may be necessary to add the following, which probably comes from
    ;;  a bug in my code...
    (defadvice vm-decode-mime-message (after u-vm-color activate)
      (u-vm-color-fontify-buffer-even-more))
    ;;  If you are using auto-fill, ie when the variable
    ;;  `vm-fill-paragraphs-containing-long-lines' is not nil, you should
    ;;  also add this:
    (defadvice vm-fill-paragraphs-containing-long-lines
        (after u-vm-color activate)
      (u-vm-color-fontify-buffer))

    (require 'vm-pine)
    ;; (require 'vm-summary-faces)
    ;; (vm-summary-faces-mode 1)
    )

  )


(provide 'sm-mail)
