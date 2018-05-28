;;
;; Vm mail reader stuff
;;
(require 'vm-autoloads)
(require 'vm-reply)

;; Make VM your default mail agent in Emacs
(setq mail-user-agent 'vm-user-agent)

(setq vm-berkeley-mail-compatibility t)
(setq vm-circular-folders nil)
(setq vm-delete-empty-folders t)
(setq vm-folder-directory "~/mail/")
(setq vm-highlighted-header-regexp "^\From\\|^Subject")
(setq vm-included-tex-prefix "   >")
(setq vm-move-after-deleting t)
(setq vm-preview-lines nil)
(setq vm-startup-with-summary 0)
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
(setq vm-stunnel-program "/usr/local/bin/stunnel")
; use IMAP instead... (setq vm-primary-inbox "~/mail/inbox")
(setq vm-primary-inbox "imap-ssl:griffon.alerce.com:23:inbox:login:hartzell:*")
(setq vm-spool-files
      '(
        "/var/mail/hartzell"
        ;; special imap3s listener running on telnet port for access from genentech.
        ;; "imap-ssl:griffon.alerce.com:23:inbox:login:hartzell:*"
        ;; special pop3s listener running on telnet port for access from genentech.
        ; "pop-ssl:griffon.alerce.com:23:pass:hartzell:*"
        ;; hit the pop3s server on the normal port
        ; "pop-ssl:griffon.alerce.com:995:pass:hartzell:*"
        ))
(setq vm-imap-account-alist
      ;; for other IMAP servers
      '(
        ;;("imap-ssl:imap.gmail.com:993:inbox:login:georgewh@gene.com:*" "gne")
        ("imap-ssl:griffon.alerce.com:993:inbox:login:hartzell:*" "hartzell@alerce.com")
        ("imap-ssl:griffon.alerce.com:993:inbox:login:hartzell@georgehartzell.com:*" "georgehartzell.com-imap")
        ))
(setq vm-imap-auto-expunge-alist
      '(("imap-ssl:griffon.alerce.com:23:inbox:login:hartzell:*" . t)))

(add-hook 'vm-mode-hook
          '(lambda ()
             (setq vm-visible-headers
                   (append vm-visible-headers (list "X-Spam-Status:")))
             (bbdb-insinuate-vm)
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
          '(lambda ()
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
;(setq send-mail-function 'smtpmail-send-it)
;(setq message-send-mail-function 'smtpmail-send-it)
;(setq smtpmail-smtp-server "localhost")
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
;(require 'vm-summary-faces)
;(vm-summary-faces-mode 1)
;
