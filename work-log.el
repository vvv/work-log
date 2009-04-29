;;; work-log.el --- work log maintenance commands for Emacs

;; Copyright (C) 2009  Valery V. Vorotyntsev <valery.vv@gmail.com>

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

(defvar work-log-date-regexp "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")

;; XXX Mark incorrectly indented lines with `font-lock-warning-face'?
(defvar work-log-font-lock-keywords
  `(("^[ \t]*#.*$" . 'font-lock-comment-face)
    (,(concat "^\\(?:" work-log-date-regexp "\\)") . 'font-lock-string-face)
    (,(concat "^" work-log-indent "[+*].*$") . 'work-log-completed)
    (,(concat "^" work-log-indent "-.*$") . 'work-log-decided-against))
  "List of keywords to highlight in Work-Log mode.")

;;;###autoload
(define-derived-mode work-log-mode text-mode "Work-Log"
  "Mode for work logs maintenance.
Inspired by John Carmak's
<http://doom-ed.com/blog/1997/10/10/last-two-months-of-work-log>.

Format:
  * entry was completed on that day
  + entry was completed on a later day
  - entry was decided against on a later day

Dates start at beginning of line. Date format is `%Y-%m-%d'
(e.g., `2009-04-29').

Lines having `#' as first non-whitespace character are comments.

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
    (goto-char (point-min))
    (let (beg end)
      (while (not (eobp))
	(setq beg (point))
	(when (re-search-forward	; "active" entry
	       (concat "^" work-log-indent "[^-+* \t#{}]")
	       nil 'move)
	  (work-log-next-date -1))
	(setq end (point))
	(cond ((< beg end)
	       (overlay-put (make-overlay beg end) 'invisible t))
	      ((> beg end)
	       (error "Infinite loop (%d-%d)" beg end)))
	(when (re-search-forward	; "inactive" entry or comment
	       (concat "^\\(" work-log-indent "[-+*]\\|[ \t]*#\\)")
	       nil 'move)
	  (beginning-of-line))))))

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
