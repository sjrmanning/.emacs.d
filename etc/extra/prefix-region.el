;;; prefix-region --- Summary
;;; Commentary:
;;
;; Once upon a time I had a little piece of elisp for a function called prefix-region.
;; Don't know where I got it, but I used it all the time.  Then I lost the source and
;; the byte compiled version stopped working.  So, I recreated it.  Wow, seems to
;; work!
;;
;; hartzell --- Sat Nov 10 10:45:28 2007
;;

;;; Code:

(defun prefix-region (beg end str)
  "Prefix the region between POINT and MARK with STRING."
  (interactive "*r\nsPrefix: ")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (while (re-search-forward "^" nil t)
      (replace-match str nil nil))
    )
  )

(provide 'prefix-region)
;;; prefix-region.el ends here
