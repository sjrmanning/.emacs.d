;;; sm-modules.el --- Configures available modules and the package manager.  -*- lexical-binding: t; -*-

;; Bootstrap elpaca.
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(defun elpaca--native-compile (e)
  "Native compile E's package."
  ;; Assumes all dependencies are 'built
  (let ((default-directory (elpaca<-build-dir e)))
    (elpaca--signal e (concat "Native compiling " default-directory) 'native-compilation)
    (elpaca--make-process e
      :name "native-compile"
      :command `(,(elpaca--emacs-path) "-Q" "-L" "."
                 ,@(cl-loop for dep in (elpaca-dependencies (elpaca<-id e) '(emacs))
                            for item = (elpaca-get dep)
                            for build-dir = (and item (elpaca<-build-dir item))
                            when build-dir append (list "-L" build-dir))
                 ;; Inherit eln load-path in child process. Otherwise, default assumed.
                 "--eval" ,(format "%S" `(setq native-comp-eln-load-path ',native-comp-eln-load-path))
                 "--batch" "-f" "batch-native-compile"
                 ,@(directory-files-recursively default-directory "\\.el$"))
      :sentinel (apply-partially #'elpaca--process-sentinel "Native compilation complete" nil))))

(setq elpaca-build-steps '(elpaca--clone elpaca--configure-remotes elpaca--checkout-ref
               elpaca--run-pre-build-commands
               elpaca--queue-dependencies elpaca--check-version
               elpaca--link-build-files
               elpaca--generate-autoloads-async elpaca--byte-compile
               elpaca--native-compile
               elpaca--compile-info elpaca--install-info
               elpaca--add-info-path elpaca--run-post-build-commands
               elpaca--activate-package))

;; Install use-package support
(setq use-package-always-ensure t)
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Block until current queue processed.
(elpaca-wait)

;; Install diminish as required by `:diminish' with use-package.
;; Note: I don't use anything special here so no need for `:delight',
;; which is slightly heavier than diminish.
(use-package diminish :defer nil)

;; `comp-run` is a built-in package.
(use-package comp-run
  :ensure nil
  :config
  (push "tramp-loaddefs.el.gz" native-comp-jit-compilation-deny-list)
  (push "cl-loaddefs.el.gz" native-comp-jit-compilation-deny-list))

(elpaca-wait)

(defvar sm/modules
  '(sm-appearance
    sm-coding-general
    sm-coding-swift
    sm-coding-web
    sm-complete
    sm-editing
    sm-navigation
    sm-notes
    sm-path
    sm-projects
    sm-searching
    sm-snippets
    sm-source-control
    sm-ui
    sm-utils
    sm-writing))

(dolist (module sm/modules)
  (require module))

(provide 'sm-modules)
