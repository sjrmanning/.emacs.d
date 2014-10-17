;;
;; File: init-defuns.el
;; Definitions for custom functions I use.

;; Smart, reusable start-or-switch from emacsredux.
(defun start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME. Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))

(defun kill-default-buffer ()
  "Kill the currently active buffer."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

;; Smart switch functions.
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (start-or-switch-to (lambda ()
                        (ansi-term (getenv "SHELL")))
                      "*ansi-term*"))

(defun visit-ielm ()
  "Switch to default `ielm' buffer.
   Start `ielm' if it's not already running."
  (interactive)
  (start-or-switch-to 'ielm "*ielm*"))

;; Switch to previous buffer.
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
   Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Smart move-to-beginning of line.
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; Quick switch to ERC buffers.
(defun erc-global-get-channel-buffer-list ()
  "Return a list of the ERC-channel-buffers"
  (erc-buffer-filter
   '(lambda()
      (if (string-match "^[^#].*:\\([0-9]*\\|ircd\\)$"
                        (buffer-name (current-buffer))) nil t)) nil))

(defun switch-to-irc ()
  "Switch to an IRC channel buffer.
   When called repeatedly, cycle through the buffers."
  (interactive)
  (let ((buffers (erc-global-get-channel-buffer-list)))
    (when (eq (current-buffer) (car buffers))
      (bury-buffer)
      (setq buffers (cdr buffers)))
    (when buffers
        (switch-to-buffer (car buffers)))))

(provide 'init-defuns)
