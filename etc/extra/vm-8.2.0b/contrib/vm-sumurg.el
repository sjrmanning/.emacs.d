;; $Header: /home/jcb/Source/Emacs/RCS/vm-sumurg.el,v 1.30 2011/12/19 14:55:59 jcb Exp $
;;; vm-sumurg.el -- Adding urgency indicators to summary
;; 
;; This file is an add-on for VM
;; 
;; Copyright (C) 2011 Julian Bradfield
;;
;; Author:      Julian Bradfield
;; Status:      Tested for VM 8.2.x running under XEmacs
;; Keywords:    VM helpers
;; X-URL:       http://homepages.inf.ed.ac.uk/jcb/Software/emacs/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


;;; Commentary:
;; This file provides an add-on to VM so that messages with certain
;; labels are tagged in bright colours, associated with urgency levels.
;; Messages labelled "*" (urgency level 1) are yellow;
;; Messages labelled "**" (urgency level 2) are orange;
;; Messages labelled "***" (urgency level 3) are red.
;; Messages labelled "****" (urgency level 4) are blinking magenta!
;; The summary modeline contains a count of the number of urgent messages.
;; A virtual folder with messages of urgency level n can be obtained
;; by V U n, or by middle-clicking on the count in the modeline.
;; It also puts a count of composition buffers in the modeline, in green,
;; to remind you that they're there.
;; In addition, messages can be set to become urgent in the future.
;;
;; The main interface is vm-sumurg-set-urgency, which see.
;; This is not bound to a key here, as VM binds all sensible keys
;; already. I bind it to * with
;; (define-key vm-mode-map "*" 'vm-sumurg-set-urgency)
;; but that overrides the default binding to vm-burst-digest.
;;
;; At one time, this worked on FSF Emacs, but I haven't tried it for
;; a long time; it's only known to work on XEmacs.

;;; Code:

(require 'vm)
(require 'vm-summary)
(require 'vm-vars)
(require 'vm-undo)
(require 'vm-folder)
(require 'vm-message)
(require 'vm-macro)
(require 'vm-misc)

; this is the list of colours associated with each urgency level.
; it is customizable only before loading---subsequent changes will
; not affect the faces used in the summary.
; It is an array indexed by urgency level. The 0th entry is used
; for hacky internal purposes.
(defvar vm-sumurg-colarray
  [ nil "yellow" "orange" "red" "magenta" ])
; colour for the composition buffer reminder
(defvar vm-sumurg-compcolor "green")

(make-face 'vm-sumurg-rightnow-face)
(set-face-background 'vm-sumurg-rightnow-face (aref vm-sumurg-colarray 4))
(set-face-foreground 'vm-sumurg-rightnow-face "white")
(set-face-blinking-p 'vm-sumurg-rightnow-face t)
(make-face 'vm-sumurg-veryurgent-face)
(set-face-background 'vm-sumurg-veryurgent-face (aref vm-sumurg-colarray 3))
(set-face-foreground 'vm-sumurg-veryurgent-face "white")
(make-face 'vm-sumurg-urgent-face)
(set-face-background 'vm-sumurg-urgent-face (aref vm-sumurg-colarray 2))
(set-face-foreground 'vm-sumurg-urgent-face "black")
(make-face 'vm-sumurg-pending-face)
(set-face-background 'vm-sumurg-pending-face (aref vm-sumurg-colarray 1))
(set-face-foreground 'vm-sumurg-pending-face "black")
(make-face 'vm-sumurg-comp-face)
(set-face-background 'vm-sumurg-comp-face vm-sumurg-compcolor)
(set-face-foreground 'vm-sumurg-comp-face "black")

; stick the faces into an array for convenience
; note that this is inserting facenames, not faces
(defconst vm-sumurg-facearray
  [ nil vm-sumurg-pending-face vm-sumurg-urgent-face
	vm-sumurg-veryurgent-face vm-sumurg-rightnow-face ]) 

; each of these symbols holds a string to go in the modeline
(defconst vm-sumurg-symarray
  [ nil vm-sumurg-modeline-pending vm-sumurg-modeline-urgent
	vm-sumurg-modeline-veryurgent vm-sumurg-modeline-rightnow ]) 


(defun vm-sumurg-level-of (m)
  (if (member "****" (vm-labels-of m)) 4
    (if (member "***" (vm-labels-of m)) 3
      (if (member "**" (vm-labels-of m)) 2
	(if (member "*" (vm-labels-of m)) 1 0)))))

; assuming that m is a message, highlight it in yellow, orange or red
; according as it has a *, **, or *** label.
(defun vm-sumurg-highlight-message ()
  (vm-sumurg-add-highlights (string-to-number (vm-number-of m))
			    (vm-su-start-of m) (vm-su-end-of m)
			    (vm-sumurg-level-of m) 
			    ))

(defadvice vm-summary-highlight-region (after vm-sumurg-vshr activate compile)
  (vm-sumurg-highlight-message))

(defvar vm-sumurg-counter [0 0 0 0 0])

(defvar vm-sumurg-comp-counter 0)
(defvar vm-sumurg-comp-counted nil)
(make-variable-buffer-local 'vm-sumurg-comp-counted)
;; This is a global (not per buffer) marker of composition buffers
(defvar vm-sumurg-modeline-comp nil)

(defun vm-sumurg-comp-hook ()
  ; in case mail-mode is switched off and on for some reason
  (if vm-sumurg-comp-counted t
    (setq vm-sumurg-comp-counter (1+ vm-sumurg-comp-counter))
    (setq vm-sumurg-comp-counted t)
    ;; set the comp entry
    (setq vm-sumurg-modeline-comp
	  (if (> vm-sumurg-comp-counter 0) 
	      (format "%d%s" vm-sumurg-comp-counter "C")))
    (redraw-modeline t)))

(add-hook 'mail-mode-hook 'vm-sumurg-comp-hook)

(defun vm-sumurg-comp-end-hook ()
  (when vm-sumurg-comp-counted
    (setq vm-sumurg-comp-counted nil)
    (setq vm-sumurg-comp-counter (1- vm-sumurg-comp-counter))
    ;; set the comp entry
    (setq vm-sumurg-modeline-comp
	  (if (> vm-sumurg-comp-counter 0) 
	      (format "%d%s" vm-sumurg-comp-counter "C")))
    (redraw-modeline t)))

(add-hook 'vm-mail-send-hook 'vm-sumurg-comp-end-hook)
(add-hook 'kill-buffer-hook 'vm-sumurg-comp-end-hook)

(defvar vm-sumurg-urgency-array nil)

(defvar vm-sumurg-default-time "00:01"
  "*The time at which urgency changes happen when no specific time is given.")

(defun vm-sumurg-set-modeline-entries ()
  ;; map across urgency levels setting the modeline entry
  ;; and noting which is the highest we have
  (let ((maxl 0) count)
    (mapcar (lambda (level)
	      (setq count (aref vm-sumurg-counter level))
	      (set (aref vm-sumurg-symarray level)
		   (if (> count 0) 
		       (format "%d%s" count (substring "****" 0 level))))
	      (if (> count 0) (setq maxl level)))
	    '( 1 2 3 4))
    ;; if there's a rightnow message, enable the blinker, else disable
    (if (eq maxl 4)
	(vm-sumurg-blinker-enable)
      (vm-sumurg-blinker-disable))
    ;; in fsfmacs, we can't set faces within the modeline, and it's
    ;; easy not to notice the urgent flag. So we set the modeline
    ;; foreground to an appropriate colour for this frame only.
    ;; This is pretty heavy-handed, but maybe better than nothing.
    ;; there seems to be no clean way to restore the original foreground.
    ;; So the summary mode hook stashes in colarray[0], which is then
    ;; right for this code.
    ;; ARGH ARGH ARGH FSF LOSSAGE :-)
    ;; this fails with virtual folders: the modeline in the frame
    ;; of the original folder isn't updated.
    ;; I can see absolutely no non-horrible solution to this.
    (if vm-fsfemacs-p
	(set-face-foreground 'modeline
			     (aref vm-sumurg-colarray maxl)
			     (selected-frame)))))

(defun vm-sumurg-add-highlights (mnum start end level)
  ;; decrement the counter for the message's previous urgency level
  (let ((olevel (aref vm-sumurg-urgency-array mnum)))
    (if (> olevel 0)
	(aset vm-sumurg-counter olevel (1- (aref vm-sumurg-counter olevel))))
    (aset vm-sumurg-urgency-array mnum level)
    (if (> level 0)
	(progn
	  (aset vm-sumurg-counter level (1+ (aref vm-sumurg-counter level)))
	  (cond (vm-xemacs-p
		 ;; re-use extents, and delete them when not required
		 (let ((e (extent-at (/ (+ start end))
				     (current-buffer) 'vm-sumurg))) 
		   ;; why not 1- end ? Because the extent is right-open
		   ;; so it gets deleted any by the summary update (see code)
		   (if e t
		     (setq e (make-extent start end))
		     (set-extent-property e 'start-open t)
		     ;; this was t. But I don't know why, and nil seems
		     ;; to avoid the problem with the selected message
		     ;; not updating.
		     (set-extent-property e 'detachable nil)
		     (set-extent-property e 'vm-sumurg t)
		     )
		   (set-extent-property e 'face 
					(aref vm-sumurg-facearray level)))) 
		(vm-fsfemacs-p
		 ;; why 1- ? Because then the overlay gets deleted by
		 ;; the process of summary update.
		 (let ((e (make-overlay start (1- end))))
		   (overlay-put e 'evaporate t)
		   (overlay-put e 'face (aref vm-sumurg-facearray level))))))
      ;; level 0: emacs, delete the extent
      (cond (vm-xemacs-p
	     (let ((e (extent-at (/ (+ start end)) 
				 (current-buffer) 'vm-sumurg))) 
	       (if e (delete-extent e))))))
    (vm-sumurg-set-modeline-entries)))

(defvar vm-sumurg-check-pending-in-progress nil)

;; this holds an obarray used to record whether a message has
;; a timer set on it
(defvar vm-sumurg-timer-obarray nil)
(make-variable-buffer-local 'vm-sumurg-timer-obarray)

;; check a message for a future urgency level, and set a timer
(defun vm-sumurg-check-future (m)
  (mapcar (lambda (label)
	    (when (string-match "^\\(\\*+\\)\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\(?:[tT]\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\)?$" label)
	      (let* ((day (string-to-number (match-string 4 label)))
		     (month (string-to-number (match-string 3 label)))
		     (year (string-to-number (match-string 2 label)))
		     (hour 0) (min 0)
		     time tmp
		     (hhmmregex "^\\([0-9][0-9]\\):\\([0-9][0-9]\\)$")
		     (now (current-time))
		     (level (- (match-end 1) (match-beginning 1)))
		     (vm-message-pointer (list m))
		     )
		(if (match-beginning 5)
		    (progn (setq hour (string-to-number (match-string 5 label)))
			   (setq min (string-to-number (match-string 6 label))))
		  (when vm-sumurg-default-time
		    (if (string-match hhmmregex vm-sumurg-default-time)
			(progn (setq hour 
				     (string-to-number 
				      (match-string 1 vm-sumurg-default-time)))
			       (setq min 
				     (string-to-number 
				      (match-string 2 vm-sumurg-default-time))))
		      (message "Trying to fix up default time %s" 
			       vm-sumurg-default-time)
		      (condition-case nil
			  (progn (setq tmp 
				       (vm-sumurg-parse-date 
					;; avoid the "add a day to early"
					(concat "+0 " vm-sumurg-default-time)))
				 (setq vm-sumurg-default-time
				       (format-time-string "%H:%M" (car tmp)))
				 (message "Fixed to %s" vm-sumurg-default-time)
				 (setq tmp (decode-time (car tmp)))
				 (setq hour (nth 2 tmp))
				 (setq min (nth 1 tmp)))
			(error (progn (message "Unable to fix - clearing")
				      (setq vm-sumurg-default-time nil)))))))
		;; it seems to be a bad move to mess with labels
		;; while rebuilding a summary, so if this is called
		;; from check-pending, we'll schedule a timeout immediately
		;; rather than actually doing the actions now.
		(setq time (encode-time 0 min hour day month year))
		(if (and (time-less-p time now) 
			 (null vm-sumurg-check-pending-in-progress))
		    (progn
		      (save-excursion 
			(vm-add-or-delete-message-labels label 1 nil))
		      ;; let's try to clear the label out of the global list
		      ;; to avoid indefinite build-up
		      (unintern (concat (vm-su-message-id m) label) 
			      vm-sumurg-timer-obarray)
		      (save-excursion
			(set-buffer (vm-buffer-of (vm-real-message-of m)))
			(unintern label vm-label-obarray))
		      (save-excursion (vm-sumurg-set-urgency level nil 1 m))
		      )
		  ;; set a timeout
		  ;; but not if there's already one set for this message
		  ;; and label
		  (when (not (and vm-sumurg-timer-obarray
				  (intern-soft 
				   (concat (vm-su-message-id m) label) 
				   vm-sumurg-timer-obarray)))
		    (if (null vm-sumurg-timer-obarray)
		      (let ((o (make-vector 29 0)))
			(setq vm-sumurg-timer-obarray o)
			;; copy it to the other buffer
			;; we expect always to be in the summary
			;; buffer here, but just in case...
			(save-excursion
			  (set-buffer (or vm-mail-buffer vm-summary-buffer))
			  (setq vm-sumurg-timer-obarray o))))
		    (intern (concat (vm-su-message-id m) label) 
			    vm-sumurg-timer-obarray)
		    (setq time (time-subtract time now))
		    (setq time (time-add time (list 0 1))) ; to avoid jiggles
		    (setq time (+ (* 65536 (car time)) (cadr time)))
		    (if (<= time 0) (setq time 0.1))
		    ;; if the time is too big to represent, set it to a week
		    ;; then it'll get re-calculated.
		    (if (> time (* 7 86400)) (setq time (* 7 86400)))
		    (message "setting timer on msg %s in %.0f seconds"
			     (vm-su-message-id m) time)
		    (add-timeout time
				 (lambda (arg)
				   (when (buffer-live-p (car arg))
				     (save-excursion
				       (set-buffer (car arg))
				       (let ((mp vm-message-list))
					 (while (and mp 
						     (not (equal 
							   (vm-message-id-of
							    (car mp))
							   (cadr arg))))
					   (setq mp (cdr mp)))
					 (if mp 
					     (vm-sumurg-check-future (car mp))))
				       (vm-follow-summary-cursor)
				       (vm-select-folder-buffer)
				       (intern (buffer-name) 
					       vm-buffers-needing-display-update)
				       (vm-update-summary-and-mode-line))))
				 (list (current-buffer) (vm-su-message-id m)))
		    ))
		)))
	    (vm-labels-of m))
)


(defconst vm-sumurg-pending-extent
  (if vm-xemacs-p
      (let ((e (make-extent nil nil))
	    (k (make-sparse-keymap)))
	(set-extent-face e 'vm-sumurg-pending-face)
	(set-extent-keymap e k)
	(set-extent-property e 'help-echo "button2 selects pending messages")
	(define-key k [(button2)]
	  (lambda () (interactive "@") (vm-sumurg-showurgent 1)))
	e
	)))

(defconst vm-sumurg-urgent-extent
  (if vm-xemacs-p
      (let ((e (make-extent nil nil))
	    (k (make-sparse-keymap)))
	(set-extent-face e 'vm-sumurg-urgent-face)
	(set-extent-keymap e k)
	(set-extent-property e 'help-echo "button2 selects urgent messages")
	(define-key k [(button2)] 
	  (lambda () (interactive "@") (vm-sumurg-showurgent 2))) 
	e
	)))

(defconst vm-sumurg-veryurgent-extent
  (if vm-xemacs-p
      (let ((e (make-extent nil nil))
	    (k (make-sparse-keymap)))
	(set-extent-face e 'vm-sumurg-veryurgent-face)
	(set-extent-keymap e k)
	(set-extent-property e 'help-echo 
			     "button2 selects very urgent messages")
	(define-key k [(button2)] 
	  (lambda () (interactive "@") (vm-sumurg-showurgent 3)))
	e
	)))


(defconst vm-sumurg-rightnow-extent
  (if vm-xemacs-p
      (let ((e (make-extent nil nil))
	    (k (make-sparse-keymap)))
	(set-extent-face e 'vm-sumurg-rightnow-face)
	(set-extent-keymap e k)
	(set-extent-property e 'help-echo 
			     "button2 selects very urgent messages")
	(define-key k [(button2)] 
	  (lambda () (interactive "@") (vm-sumurg-showurgent 4)))
	e
	)))


(defconst vm-sumurg-comp-extent
  (if vm-xemacs-p
      (let ((e (make-extent nil nil))
	    (k (make-sparse-keymap)))
	(set-extent-face e 'vm-sumurg-comp-face)
	(set-extent-keymap e k)
	(set-extent-property e 'help-echo 
			     "button2 switches to a composition buffer")
	(define-key k [(button2)] 
	  (lambda () (interactive) (vm-continue-composing-message)))
	e
	)))


; modeline element for xemacs
(defvar vm-sumurg-modeline-element
  (cond (vm-xemacs-p
	 (list
	  (list 'vm-sumurg-modeline-comp
		(list vm-sumurg-comp-extent "" 
		      'vm-sumurg-modeline-comp))
	  (list 'vm-sumurg-modeline-pending
		(list vm-sumurg-pending-extent "" 
		      'vm-sumurg-modeline-pending))
	  (list 'vm-sumurg-modeline-urgent
		(list vm-sumurg-urgent-extent "" 
		      'vm-sumurg-modeline-urgent))
	  (list 'vm-sumurg-modeline-veryurgent
		(list vm-sumurg-veryurgent-extent "" 
		      'vm-sumurg-modeline-veryurgent))
	  (list 'vm-sumurg-modeline-rightnow
		(list vm-sumurg-rightnow-extent "" 
		      'vm-sumurg-modeline-rightnow))))
	(vm-fsfemacs-p
	 (list
	  (list 'vm-sumurg-modeline-comp
		(list "" 'vm-sumurg-modeline-comp))
	  (list 'vm-sumurg-modeline-pending
		(list "" 'vm-sumurg-modeline-pending))
	  (list 'vm-sumurg-modeline-urgent
		(list "" 'vm-sumurg-modeline-urgent))
	  (list 'vm-sumurg-modeline-veryurgent
		(list "" 'vm-sumurg-modeline-veryurgent))
	  (list 'vm-sumurg-modeline-rightnow
		(list "" 'vm-sumurg-modeline-rightnow))
))))


; stick it at the end
(add-hook 'vm-summary-mode-hook
	(if vm-xemacs-p
	    (lambda ()
	      (setq vm-sumurg-counter (vector 0 0 0 0 0))
	      (if (memq vm-sumurg-modeline-element modeline-format)
		  t
		(setq modeline-format
		      (append modeline-format vm-sumurg-modeline-element))))
	  (lambda ()
	    (aset vm-sumurg-colarray 0 (face-foreground 'modeline))
	    (setq vm-sumurg-counter (vector 0 0 0 0 0))
	    (setq mode-line-format
		  (append mode-line-format vm-sumurg-modeline-element)))))

(make-variable-buffer-local 'vm-sumurg-counter)
(make-variable-buffer-local 'vm-sumurg-modeline-pending)
(make-variable-buffer-local 'vm-sumurg-modeline-urgent)
(make-variable-buffer-local 'vm-sumurg-modeline-veryurgent)
(make-variable-buffer-local 'vm-sumurg-modeline-rightnow)
(make-variable-buffer-local 'vm-sumurg-urgency-array)

;; takes a modeline format, and returns the same with any 
;; substantive occurrence of vm-ml-labels prefixed by 
;; the extent (at function call time) vm-ml-sumurg-extent

(defvar vm-ml-sumurg-extent nil)
(make-variable-buffer-local 'vm-ml-sumurg-extent)
(defun vm-sumurg-munge-modeline (x)
  (if (consp x)
      (cons (car x) (mapcar 'vm-sumurg-munge-modeline (cdr x)))
    (if (eq x 'vm-ml-labels)
	(list vm-ml-sumurg-extent "" 'vm-ml-labels)
      x)))

;; hook into vm mode to set the modeline format
(defun vm-sumurg-vm-mode-hook-fn ()
  (setq vm-ml-sumurg-extent (make-extent nil nil))
  (setq modeline-format (vm-sumurg-munge-modeline modeline-format)))

(add-hook 'vm-mode-hook ' vm-sumurg-vm-mode-hook-fn)
(add-hook 'vm-presentation-mode-hook ' vm-sumurg-vm-mode-hook-fn)

(require 'advice)

(defadvice  vm-do-needed-mode-line-update
  (before vm-sumurg-dnmlu activate compile)
  (when (and vm-message-pointer vm-ml-sumurg-extent)
    (set-extent-face vm-ml-sumurg-extent 
	  (aref vm-sumurg-facearray 
		(vm-sumurg-level-of (car vm-message-pointer))))
    (if vm-presentation-buffer
	(save-excursion
	  (set-buffer vm-presentation-buffer)
	  (set-extent-face vm-ml-sumurg-extent 
			   (aref vm-sumurg-facearray 
				 (vm-sumurg-level-of 
				  (car vm-message-pointer))))))))


; given a pointer into a message list, return the first element
(defun vm-first (mp)
  (let (prev)
    (while (setq prev (vm-reverse-link-of (car mp)))
      (setq mp prev))
    mp))


; this assumes that m-list points to the message list being summarized
(defun vm-sumurg-check-pending ()
  (let ((vm-sumurg-check-pending-in-progress t))
    (if (null m-list)
	(vm-sumurg-set-modeline-entries)
      (let* ((this (string-to-number (vm-number-of (car m-list))))
	    (last (string-to-number (vm-number-of (car (vm-last m-list)))))
	    (curlen (length vm-sumurg-urgency-array))
	    (newlen (1+ last))
	    i l
	    )
	(when (> newlen curlen)
	  (setq newlen (+ newlen (/ newlen 20)))
	  (setq vm-sumurg-urgency-array
		(vconcat vm-sumurg-urgency-array
			 (make-vector (- newlen curlen) 0))))
	(setq i this)
	(while (< i newlen)
	  (setq l (aref vm-sumurg-urgency-array i))
	  (when (> l 0)
	    (aset vm-sumurg-counter l (1- (aref vm-sumurg-counter l)))
	    (aset vm-sumurg-urgency-array i 0))
	  (setq i (1+ i)))
	(mapcar (lambda (m) 
		  (vm-sumurg-check-future m) (vm-sumurg-highlight-message))
		m-list)))))
  
(add-hook 'vm-summary-update-hook 'vm-sumurg-highlight-message)
(add-hook 'vm-summary-redo-hook 'vm-sumurg-check-pending)

; code for blinking the rightnow messages
(defvar vm-sumurg-blinker-needed nil)
(make-variable-buffer-local 'vm-sumurg-blinker-needed)
(defvar vm-sumurg-blinker-blink nil)
(defvar vm-sumurg-blinker-timeout-id nil)
(defvar vm-sumurg-blinker-in-focus nil)
(defun vm-sumurg-blinker-callback (junk)
  (if vm-sumurg-blinker-in-focus
      (if vm-sumurg-blinker-blink
	  (progn (setq vm-sumurg-blinker-blink nil)
		 (set-face-background 'vm-sumurg-rightnow-face "magenta"))
	(setq vm-sumurg-blinker-blink t)
	(set-face-background 'vm-sumurg-rightnow-face "cyan"))
    (disable-timeout vm-sumurg-blinker-timeout-id)
    (setq vm-sumurg-blinker-timeout-id nil)
    (setq vm-sumurg-blinker-blink nil)
    (set-face-background 'vm-sumurg-rightnow-face "magenta")))
(defun vm-sumurg-blinker-select-frame-hook ()
  (setq vm-sumurg-blinker-in-focus
	(and (eq (frame-type (selected-frame)) 'x) vm-sumurg-blinker-needed))
  (if (and vm-sumurg-blinker-in-focus
	   (null vm-sumurg-blinker-timeout-id))
      (setq vm-sumurg-blinker-timeout-id
	    (add-timeout 1 'vm-sumurg-blinker-callback nil 1))))
(defun vm-sumurg-blinker-deselect-frame-hook ()
  (setq vm-sumurg-blinker-in-focus nil))
(defun vm-sumurg-blinker-enable ()
  (setq vm-sumurg-blinker-needed t)
  (if vm-mail-buffer 
      (vm-copy-local-variables vm-mail-buffer 'vm-sumurg-blinker-needed))
  (if vm-presentation-buffer 
      (vm-copy-local-variables vm-presentation-buffer 
			       'vm-sumurg-blinker-needed))
  (add-hook 'select-frame-hook 'vm-sumurg-blinker-select-frame-hook)
  (add-hook 'deselect-frame-hook 'vm-sumurg-blinker-deselect-frame-hook)
  (vm-sumurg-blinker-select-frame-hook))
(defun vm-sumurg-blinker-disable ()
  (remove-hook 'select-frame-hook 'vm-sumurg-blinker-select-frame-hook)
  (remove-hook 'deselect-frame-hook 'vm-sumurg-blinker-deselect-frame-hook)
  (setq vm-sumurg-blinker-in-focus nil)
  (setq vm-sumurg-blinker-needed nil)
  (if vm-mail-buffer 
      (vm-copy-local-variables vm-mail-buffer 'vm-sumurg-blinker-needed))
  (if vm-presentation-buffer 
      (vm-copy-local-variables vm-presentation-buffer 
			       'vm-sumurg-blinker-needed))
  )

; bound to vm-virtual-folder-alist in following command
(defvar vm-sumurg-urgent-folder-alist
        '(
          ;; start virtual folder definition
          ("pending"
           (nil ; no real folder
            (label "*")
	    (label "**")
	    (label "***")
	    (label "****")
	    ))
          ("urgent"
           (nil ; no real folder
	    (label "**")
	    (label "***")
	    (label "****")
           ))
          ("very urgent"
           (nil ; no real folder
	    (label "***")
	    (label "****")
           ))
          ("right now!"
           (nil ; no real folder
	    (label "****")
           ))
	  )
	)


; set urgency level: clears other labels of different urgencies
; This is not bound to a key here, because I can't think of the
; right keybinding. I use *, but that's vm-burst-digest standardly.
(defun vm-sumurg-set-urgency (level &optional date count msg clear)
  "*Set the urgency level of a message.
Interactively, this prompts for an urgency level from 0 (unmarked) to 4,
and sets the message's urgency accordingly.

A numeric prefix argument is treated in the usual way, setting the
following N messages to the given urgency level.

If called with a simple prefix argument (C-u), it first prompts
for a date on which the message is to be set to the given urgency level.
If called with a double prefix argument (C-u C-u), it clears any pending
urgency changes on the message.

The date can be given in several reasonable forms:

ISO: 2012-01-22
European numeric: 22/01/2012 or 22/01/12
British traditional: 22 January 2012  or  22 Jan 2012  or  Jan 22, 2012
 (month names can be given either in full, or as the first three letters)
Except in ISO format, the year can be omitted, and the next such date will
be assumed.

For the next few days, there are two options: a weekday name, which may be
given in full, or with the first three, or first two, letters. It may be
followed by  week  (or  wk  for the really lazy), to add another seven days.
For example:
 monday
 tue
 wed week

Alternatively, a number of days in the future may be given by +N:
+1  tomorrow
+2  day after tomorrow
+   tomorrow (N.B. means +1 not +0)

Any date spec may be preceded or followed by a time spec, in several
reasonable formats: 19:27  19.27  19h27  7.27 pm  19h  7pm.
Specifically any of h : . is recognized as a separator; am and pm are 
recognized in either case and with or without full stops; the separator
and minutes may be omitted, provided that h or am/pm is used.
 (To avoid confusion with years, military format 1927 is not accepted.)

A time spec normally means that time on the given date. In the special case 
where there is only a time spec, and the date is empty, it means the next
occurrence of that time: e.g. at 19:00, a date/time spec of 09:00 means
the following morning.

A date spec without a time spec will become active according to the value
of `vm-sumurg-default-time', which should be a string containing a time
in any of the above formats. This defaults to \"00:01\"; it might be useful to
set it to, say, \"08:30\", so that messages don't become urgent until you get
to the office! (Note: the value of `vm-sumurg-default-time' that counts is
that when the urgency is set, or when VM loads the mail folder, whichever
happens later.)"

  (interactive 
   (let ((prompt "Urgency level (0-4): ")
	 level date timep count clear)
     (when (consp current-prefix-arg)
       (if (= (prefix-numeric-value current-prefix-arg) 16)
	   (setq clear t)
	 (setq date (vm-sumurg-parse-date (read-string "Date to set: ")))
	 (setq timep (cadr date))
	 (setq date (format-time-string 
		     (if timep "%Y-%m-%dT%H:%M" "%Y-%m-%d")
		     (car date)))
	 (setq prompt (concat "On " date " set urgency level (1-4): ")))
       (setq current-prefix-arg nil))
     (if clear t (setq level (read-number prompt t)))
     (setq count (prefix-numeric-value current-prefix-arg))
     (list level date count nil clear)))
  (if (null count) (setq count 1))
  (if (and (not clear) (or (< level 0) (> level 4)))
      (error "%d is not a known urgency level" level))
  (when (null msg)
    (vm-follow-summary-cursor))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-read-only)
  (vm-error-if-folder-empty)
  (let ((vm-message-pointer vm-message-pointer))
    (if msg (setq vm-message-pointer (list msg)))
    (if clear
	(mapcar (lambda (label)
		  (when (string-match "^\\*+[-0-9:t]+$" label)
		    (vm-add-or-delete-message-labels label count nil)
		    (save-excursion
		      (set-buffer (vm-buffer-of (vm-real-message-of 
                        (car vm-message-pointer))))
		      (unintern label vm-label-obarray))))
		(vm-labels-of (car vm-message-pointer)))
      (if date 
	  (progn
	    (vm-add-or-delete-message-labels
	     (concat (substring "****" 0 level) date) count 'all)
	    (vm-sumurg-check-future (car vm-message-pointer)))
	(vm-add-or-delete-message-labels 
	 "* ** *** ****" count nil)
	(vm-add-or-delete-message-labels 
	 (substring "****" 0 level) count 'all)))
;    ;; for reasons I don't understand, the display of the selected message
;    ;; doesn't get updated - some interaction with the highlight face,
;    ;; I guess. So call highlight message explicitly 
;    (let ((m (car vm-message-pointer)))
;      (save-excursion
;	(set-buffer vm-summary-buffer)
;	(vm-sumurg-highlight-message))))
;    (let ((modified (buffer-modified-p)))
;      (set-buffer-modified-p t)
;      (vm-do-needed-mode-line-update)
;      (set-buffer-modified-p modified))
))

; form a buffer with pending messages
(defun vm-sumurg-showurgent (level)
  "Make a virtual folder containing messages whose urgency is greater
than or equal to the given value (prompted for, when interactive)."
  (interactive "nUrgency level (1-4): ")
  (if (or (< level 1) (> level 4)) (error "%d is not a known urgency level"))
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-error-if-folder-empty)
  (let ((vm-virtual-folder-alist vm-sumurg-urgent-folder-alist)
	; scream. Problem1: these folder defns get installed
	; on the menu bar; problem2: vm-install-known-virtual-folders-menu
	; doesn't handle empty alists correctly, so we can't
	; just run it again after exiting the let form
	; (and anyway, we don't want them on the new folder's menu)
	; Therefore: hackety hack HACK:
	; somebody tell me how to do this right
	(keepfn (symbol-function 'vm-menu-install-known-virtual-folders-menu)))
    (unwind-protect
	(progn
	  (fset 'vm-menu-install-known-virtual-folders-menu (lambda () nil))
	  (vm-apply-virtual-folder
	   (cond ((= level 1) "pending")
		 ((= level 2) "urgent")
		 ((= level 3) "very urgent")
		 ((= level 4) "right now!")
		 )))
      (fset 'vm-menu-install-known-virtual-folders-menu keepfn))))


(define-key vm-mode-map "VU" 'vm-sumurg-showurgent)

; add item to the virtual menu
(require 'vm-menu)

(let ((mp vm-menu-virtual-menu)
      mprev
      (item (vector "Make Urgent Virtual Folder" 'vm-sumurg-showurgent t)))
  (while (and mp (or (not (stringp (car mp)))
		     (not (string-match "^--*$" (car mp)))))
    (setq mprev mp)
    (setq mp (cdr mp)))
  (if mprev
      (setcdr mprev (cons item mp))
    (setq vm-menu-virtual-menu (cons item mp))))


; routines to parse dates in a reasonable format.
; Returns a list (TIME TIMEP).
; The TIME is a time value, corresponding to the given date string.
; If the date string contains no time specifier, the time is zero hours,
; and TIMEP is nil. If the date string contained a time value, 
; TIMEP is t.
; Time specifiers have the form hh:mm or hh.mm, optionally 
; preceded by a T (for the benefit of ISO format), and optionally
; followed by am or pm (which we handle correctly).


(defun vm-sumurg-parse-date (s)
  (let ((now (current-time))
	(case-fold-search t)
	(ts)
	(hh 0) (mm 0) (xm) (timep)
	(timeregexp (eval-when-compile 
	     (concat
	      ;; XEmacs bug: ^ not recognized after shy group open, so
	      ;; put it as second alternative
	      "\\(?:[_t]\\|^\\|\\s-\\)\\s-*" ; start with beginning, T
					; or whitespace 
	      "\\([0-9][0-9]?\\)" ; the hours
	      ;; now either we have minutes followed by an optional
	      ;; am/pm, or we have a compulsory h/am/pm
	      ;; open, and first alternative
	      "\\(?:[h:.]\\([0-9][0-9]\\)\\s-*\\(?:\\([ap]\\)\\.?m\\.?\\)?"
	      ;; second alternative
	      "\\|\\s-*\\(?:h\\|\\([ap]\\)\\.?m\\.?\\)\\)"))
)
	(date))
    (setq date (decode-time now))
    ;; look for and remove a time string 
    ;; we want to allow either minutes or h/am/pm to be omitted,
    ;; but not both. Unfortunately, there's no way to avoid writing
    ;; the pm/am twice.
    (when (or (when (string-match timeregexp s)
		(setq ts s) 
		(setq s (replace-match "" nil nil s))
		(setq timep t))
	      (and vm-sumurg-default-time
		   (setq ts vm-sumurg-default-time)
		   (or (string-match timeregexp ts)
		       (error 'invalid-argument 
			      "vm-sumurg-default-time not in a valid time format"
			      'vm-sumurg-default-time))))
      (setq hh (string-to-number (match-string 1 ts)))
      (if (match-beginning 2)
	  (setq mm (string-to-number (match-string 2 ts))))
      (setq xm (or (match-string 3 ts) (match-string 4 ts)))
      (when xm
	(if (or (equal xm "a") (equal xm "A"))
	    (if (equal hh 12) (setq hh 0))
	  (if (< hh 12) (setq hh (+ hh 12))))))
    (list (apply 'encode-time
	   (cond 
	    ;; +n or empty
	    ((string-match "^\\s-*\\(?:\\+\\([0-9]*\\)\\)?\\s-*$" s)
	     (list 0 mm hh
		   (+ (nth 3 date)
		      (if (null (match-beginning 1))
			  ;; empty, today. If a time was given, and
			  ;; it's before now, then make it tomorrow
			  (if (and timep
				   (or (< hh (nth 2 date))
				       (and (= hh (nth 2 date))
					    (< mm (nth 1 date)))))
			      1
			    0)
			(if (equal (match-beginning 1) (match-end 1)) 1
			  (string-to-number (match-string 1 s)))))
		   (nth 4 date)
		   (nth 5 date)))
	    ;; fooday week
	    ((string-match
	     (eval-when-compile (concat "^\\s-*" ; white space at beginning
		      "\\(?:" ; start of day match
		      ;; given a string, build a regexp that matches
		      ;; the first 2 letters, 
		      ;; the first three letters, or the whole
		      ;; string. I.e. monday will 
		      ;; match mo, mon, monday
		      (mapconcat 
		       (lambda (d)
			 (concat "\\(" (substring d 0 2) 
				 "\\(?:" (substring d 2 3)
				 "\\(?:" (substring d 3) "\\)?\\)?\\)")
			 )
		       '("sunday" "monday" "tuesday" "wednesday"
			 "thursday" "friday" "saturday") "\\|")
		      "\\)" ; end of day match
		      "\\s-*" ; white space
		      "\\(w\\(?:ee\\)?k\\)?" ; week match
		      "\\s-*$" ; white space to end
		      )) ; end of constructed regexp
	     s)
	     (let* ((week (match-string 8 s))
		   (wdaynum (let ((i 1))
			      (while (and (< i 8) (null (match-beginning i)))
				(setq i (1+ i)))
			      (if (>= i 8) nil (1- i))))
		   (todaynum (nth 6 date))
		   )
	       (if (> wdaynum todaynum)
		   t ; do nothing: the day's coming up
		 (setq wdaynum (+ 7 wdaynum)))
	       (if week (setq wdaynum (+ 7 wdaynum)))
	       (list 0 mm hh (+ (- wdaynum todaynum) (nth 3 date)) (nth 4 date)
		     (nth 5 date))
	       )) ; end of first clause
	   ;; iso date, nice and easy
	   ((string-match 
	     "^\\s-*\\([12][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)\\s-*$"
	     s)
	    (list 0 mm hh (string-to-number (match-string 3 s))
		  (string-to-number (match-string 2 s))
		  (string-to-number (match-string 1 s))))
	   ;; and traditional dates
	   ((string-match 
	     "^\\s-*\\([0-9][0-9]?\\)/\\([0-9][0-9]?\\)\\(?:/\\([0-9]+\\)\\)?\\s-*$"
	     s)
	    (let ((d (string-to-number (match-string 1 s)))
		  (m (string-to-number (match-string 2 s)))
		  (y (if (match-beginning 3) 
			 (string-to-number (match-string 3 s)))))
	      (when (null y)
		  (setq y (nth 5 date))
		  (if (or (< m (nth 4 date))
			  (and (= m (nth 4 date)) (<= d (nth 3 date))))
		      (setq y (1+ y))))
	      (if (< y 100) (setq y (+ 2000 y)))
	      (list 0 mm hh d m y)))
	   ;; 5 jan yy
	   ((string-match 
	     (eval-when-compile (concat
	       "^\\s-*\\([0-9][0-9]?\\)\\s-*\\(?:" ; initial white
					; space and number 
	       ;; construct a month matcher: bracket n is month n-1
	       (mapconcat
		(lambda (m) 
		  (concat "\\(" (substring m 0 3) 
			  "\\(?:" (substring m 3) "\\)?\\)"))
		'("january" "february" "march" "april" "may" "june" "july"
		  "august" "september" "october" "november" "december")
		"\\|")
	       "\\)\\s-*\\([0-9]+\\)?\\s-*$" ; end of month group, and year
	       )) s)
	    (let ((d (string-to-number (match-string 1 s)))
		  (m (let ((i 2))
		       (while (and (< i 14) (null (match-beginning i)))
			 (setq i (1+ i)))
		       (if (>= i 14)
			   (error 'internal-error 
				  "matched impossible month in vm-sumurg-parse-date")
			 (1- i))))
		  (y (if (match-beginning 14)
			 (string-to-number (match-string 14 s)))))
	      (when (null y)
		  (setq y (nth 5 date))
		  (if (or (< m (nth 4 date))
			  (and (= m (nth 4 date)) (<= d (nth 3 date))))
		      (setq y (1+ y))))
	      (if (< y 100) (setq y (+ 2000 y)))
	      (list 0 mm hh d m y)))
	   ;; and the same, for jan 5, yy
	   ((string-match 
	     (eval-when-compile (concat
	       "^\\s-*\\(?:" ; initial white space 
	       ;; construct a month matcher: bracket n is month n-1
	       (mapconcat
		(lambda (m) 
		  (concat "\\(" (substring m 0 3) 
			  "\\(?:" (substring m 3) "\\)?\\)"))
		'("january" "february" "march" "april" "may" "june" "july"
		  "august" "september" "october" "november" "december")
		"\\|")
	       "\\)\\s-*\\([0-9][0-9]?\\)\\(?:,\\s-*\\([0-9]+\\)\\)?\\s-*$" 
					; end of month group, day and year
	       )) s)
	    (let ((d (string-to-number (match-string 13 s)))
		  (m (let ((i 1))
		       (while (and (< i 13) (null (match-beginning i)))
			 (setq i (1+ i)))
		       (if (>= i 13)
			   (error 'internal-error 
				  "matched impossible month in vm-sumurg-parse-date")
			 i)))
		  (y (if (match-beginning 14)
			 (string-to-number (match-string 14 s)))))
	      (when (null y)
		  (setq y (nth 5 date))
		  (if (or (< m (nth 4 date))
			  (and (= m (nth 4 date)) (<= d (nth 3 date))))
		      (setq y (1+ y))))
	      (if (< y 100) (setq y (+ 2000 y)))
	      (list 0 mm hh d m y)))
	   (t 
	    (error 'invalid-argument 
		   (concat s " is not a recognized date format")))) 
					; end of cond
    ) timep))) ; end of defn


(provide 'vm-sumurg)
