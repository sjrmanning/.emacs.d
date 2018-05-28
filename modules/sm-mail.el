;;; sm-mail.el --- VM and BBDB and oh my!

(use-package bbdb-autoloads
  :straight nil
  :load-path "etc/extra/bbdb"
  :commands (bbdb bbdb-insinuate-vm)
  :init
  (progn
    (add-hook 'bbdb-load-hook
              '(lambda ()
                 (setq bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook)
                 (setq bbdb-change-hook 'bbdb-timestamp-hook)
                 (setq bbdb-completion-type 'primary-or-name)
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
                 ))
    ))

(add-to-list 'load-path (sm/emacs.d "etc/extra/vm-8.2.0b"))
(add-to-list 'load-path (sm/emacs.d "etc/extra/vm-8.2.0b/lisp"))
(load (sm/emacs.d "etc/extra/my-setup-mail.el"))

(provide 'sm-mail)
