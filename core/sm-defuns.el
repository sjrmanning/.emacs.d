;;; init-defuns.el --- Custom functions required by other init files.

;; Function for determining emacs dir paths.
(defun sm/emacs.d (path)
  "Return path inside user's `.emacs.d'."
  (expand-file-name path user-emacs-directory))

(defun sm/cache-for (identifier)
  "Return cache directory for given identifier."
  (expand-file-name identifier (sm/emacs.d "var/cache")))

(defun sm/load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (sm/load-directory fullpath))
       ((and (eq isdir nil)
             (string= (substring path -3) ".el")
             (not (string-match "^\\." path)))
        (load (file-name-sans-extension fullpath)))))))

;; Add yasnippet support for company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defun sm/backend-with-yas (backend)
  (append (if (consp backend) backend (list backend))
          '(:with company-yasnippet)))

(provide 'sm-defuns)
