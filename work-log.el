;;; work-log.el --- Emacs mode for working with TODO lists formatted
;;; "à la John Carmack"

;; Copyright (C) 2009, 2010  Valery V. Vorotyntsev <valery.vv@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; This file is not part of GNU Emacs.

(defgroup work-log nil
  "Work log maintenance."
  :group 'convenience
  :version "22.3.1")

(defface work-log-completed
  '((t (:inherit shadow)))
  "Face for completed entries."
  :group 'work-log)

(defface work-log-decided-against
  '((t (:inherit shadow :strike-through t)))
  "Face for entries that were decided against on a later day."
  :group 'work-log)

(defvar work-log-indent "  "
  "Whitespace to precede log entry with.")

(defvar work-log-date-regexp "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")

;; XXX Mark incorrectly indented lines with `font-lock-warning-face'?
(defvar work-log-font-lock-keywords
  `(("^[ \t]*#.*$" . 'font-lock-comment-face)
    (,work-log-date-regexp . 'font-lock-string-face)
    (,(concat "^" work-log-indent "[+*].*$") . 'work-log-completed)
    (,(concat "^" work-log-indent "-.*$") . 'work-log-decided-against))
  "List of keywords to highlight in Work-Log mode.")

;;;###autoload
(define-derived-mode work-log-mode text-mode "Work-Log"
  "Mode for working with TODO lists formatted ``à la John Carmack''.

Format:
  * entry was completed on that day
  + entry was completed on a later day
  - entry was decided against on a later day

Dates start at beginning of line. Date format is `%Y-%m-%d'
(e.g., `2009-04-29').

Lines having `#' as first non-whitespace character are comments.

> When I accomplish something, I write a * line that day.
>
> Whenever a bug / missing feature is mentioned during the day and I
> don't fix it, I make a note of it. Some things get noted many times
> before they get fixed.
>
> Occasionally I go back through the old notes and mark with a + the
> things I have since fixed.

See also
    http://doom-ed.com/blog/1997/10/10/last-two-months-of-work-log
    http://www.team5150.com/~andrew/carmack/plan.html

\\{work-log-mode-map}"
  (set (make-local-variable 'comment-start) "#") ; enable `M-;'
  (set (make-local-variable 'font-lock-defaults)
       '(work-log-font-lock-keywords t nil nil nil)))

(defun work-log-new-entry ()
  "Add new log entry for today."
  (interactive)
  (goto-char (point-min))

  ;; If file starts with comments, skip them.
  ;; Assume they end at first blank line.
  (when (looking-at "[ \t]*#")
    (search-forward "\n\n")
    (skip-chars-forward "\n"))

  (let ((date (format-time-string "%Y-%m-%d")))
    (if (looking-at (regexp-quote date))
	(forward-line 1)
      (insert date "\n\n\n")
      (forward-line -2)))
  (insert "\n  "))

(defun work-log-next-date (arg)
  "Move to the next [XXX visible] date in log.
With argument, repeat or move backwards if negative."
  (interactive "p")
  (if (< arg 0) (beginning-of-line) (end-of-line))
  (let (found-p)
    (while (and (not (bobp)) (< arg 0))
      (setq found-p (re-search-backward work-log-date-regexp nil 'move)
	    arg (1+ arg)))
    (while (and (not (eobp)) (> arg 0))
      (setq found-p (re-search-forward work-log-date-regexp nil 'move)
	    arg (1- arg)))
    (when found-p (beginning-of-line))))

(defun work-log-previous-date (arg)
  "Move to the previous [XXX visible] date in log.
With argument, repeat or move forwards if negative."
  (interactive "p")
  (work-log-next-date (- arg)))

(defun work-log-hide-inactive ()
  "Hide everything except of pending (active) entries and their dates.
Make completed entries, those decided against, and comments invisible."
  (interactive)
  (remove-overlays)
  (save-excursion
    (let (;; stored position(s)
	  si   ; start of interactive region
	  sde  ; two positions around a date: (START-OF-DATE . END-OF-DATE)

	  ;; regexps
	  (active-entry (concat work-log-indent "[^-+* \t#{}]"))
	  (inactive-entry-or-comment
	   (concat "\\(?:" work-log-indent "[-+*]\\|[ \t]*#\\)"))

	  ;; helper functions
	  (hide (lambda (beg end)
		  (unless (eq beg end)
		    (overlay-put (make-overlay beg end) 'invisible t))))
;; 		    (overlay-put (make-overlay beg end)
;; 				 'face '(:inverse-video t)))))
	  (next-line-pos (lambda () (save-excursion (forward-line) (point)))))

      (goto-char (point-min))
      (while (not (eobp))
	(cond ((looking-at inactive-entry-or-comment)
	       (unless si (setq si (point))))

	      ((looking-at work-log-date-regexp)
	       (when sde
		 ;; XXX This solution is suboptimal -- unneeded
		 ;; overlays get created (e.g., 3 overlays for
		 ;; "adidida" region where 1 would suffice).
		 ;; Good news: no more "hanging" dates.
		 (funcall hide (car sde) (point)))
	       (setq sde (cons (point) (funcall next-line-pos))))

	      ((looking-at active-entry)
	       (cond (si
		      (if (not sde)
			  (funcall hide si (point))
			(when (< si (car sde))
			  (funcall hide si (car sde)))
			(funcall hide (cdr sde) (point))
			(setq sde nil))
		       (setq si nil))
		     (sde
		      (setq sde nil)))))

	(forward-line))

      ;; end of buffer
      (when si
	(if (and sde (< (car sde) si))
	    (funcall hide (car sde) (point))
	  (funcall hide si (point)))))))

(when nil ; XXX --------------------------------------------------------
(defun foo ()
  "Hide everything except of pending (active) entries and their dates.
Make completed entries, those decided against, and comments invisible."
  (interactive)
  (remove-overlays)
  (save-excursion
    (let (;; stored position(s)
	  si   ; start of interactive region
	  sde  ; two positions around a date: (START-OF-DATE . END-OF-DATE)

	  ;; regexps
;; 	  (active-entry (concat work-log-indent "[^-+* \t#{}]"))
;; 	  (inactive-entry-or-comment
;; 	   (concat "\\(?:" work-log-indent "[-+*]\\|[ \t]*#\\)"))
	  (active-entry "a")
	  (inactive-entry-or-comment "i")
	  (work-log-date-regexp "d") ; XXX

	  ;; helper functions
	  (hide (lambda (beg end)
		  (unless (eq beg end)
;; 		    (overlay-put (make-overlay beg end) 'invisible t))))
		    (overlay-put (make-overlay beg end)
				 'face '(:inverse-video t)))))
;; 	  (next-line-pos (lambda () (save-excursion (forward-line) (point)))))
	  (next-line-pos (lambda () (save-excursion (forward-char) (point)))))

      (goto-char (point-min))
      (while (not (eobp))
	(cond ((looking-at inactive-entry-or-comment)
	       (unless si (setq si (point))))

	      ((looking-at work-log-date-regexp)
	       (when sde
		 ;; XXX This solution is suboptimal -- unneeded
		 ;; overlays get created (e.g., 3 overlays for
		 ;; "adidida" region where 1 would suffice).
		 ;; Good news: no more "hanging" dates.
		 (funcall hide (car sde) (point)))
	       (setq sde (cons (point) (funcall next-line-pos))))

	      ((looking-at active-entry)
	       (cond (si
		      (if (not sde)
			  (funcall hide si (point))
			(when (< si (car sde))
			  (funcall hide si (car sde)))
			(funcall hide (cdr sde) (point))
			(setq sde nil))
		       (setq si nil))
		     (sde
		      (setq sde nil)))))

;; 	(forward-line))
	(forward-char))

      ;; end of buffer
      (when si
	(if (and sde (< (car sde) si))
	    (funcall hide (car sde) (point))
	  (funcall hide si (point)))))))
) ; XXX ----------------------------------------------------------------

(defun work-log-show-inactive ()
  "Show any hidden text.
See also `work-log-hide-inactive'."
  (interactive)
  (remove-overlays))

;;;; Key bindings. XXX DOCUMENTME
(mapc
 (lambda (binding)
   (dolist (c '(nil "\\C-"))
     (define-key work-log-mode-map
       (car (read-from-string (concat "\"\\C-c" c (car binding) "\"")))
       (intern (concat "work-log-" (symbol-name (cadr binding)))))))
 '(("e" new-entry) ; ==> `work-log-new-entry' is bound to `C-c C-e', `C-c e'
   ("n" next-date) ("p" previous-date)
   ("h" hide-inactive) ("s" show-inactive)))

(provide 'work-log)
