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

(defvar work-log-date-regexp "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")

(defvar work-log-font-lock-keywords
  `(("^[ \t]*#.*$" . 'font-lock-comment-face)
    (,(concat "^\\(?:" work-log-date-regexp "\\)") . 'font-lock-string-face)
    ("^[ \t]+[+*] .*$" . 'work-log-completed)
    ("^[ \t]+- .*$" . 'work-log-decided-against))
  "List of keywords to highlight in Work-Log mode.")

;;;###autoload
(define-derived-mode work-log-mode text-mode "Work-Log"
  "Mode for work logs maintenance.
Format:
  * entry was completed on that day
  + entry was completed on a later day
  - entry was decided against on a later day

Inspired by John Carmak's
<http://doom-ed.com/blog/1997/10/10/last-two-months-of-work-log>.

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

(define-key work-log-mode-map (kbd "C-c e") 'work-log-new-entry)
(define-key work-log-mode-map (kbd "C-c n") 'work-log-next-date)
(define-key work-log-mode-map (kbd "C-c p") 'work-log-previous-date)

(provide 'work-log)

;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
(when nil
(defun work-log-hide-inactive ()
  "Hide everything except of pending entries and their dates.
Make completed entries, those decided against, and comments
invisible."
  (interactive)
  (remove-overlays)
  (save-excursion
    (let ((beg (goto-char (point-min)))
	  (end (re-search-forward "^[ \t]+[^-+* \t].*$" nil t)))
      (when end
	(setq end (re-search-backward work-log-date-regexp beg t)))
      (overlay-put (make-overlay beg end) 'invisible t))))
)

;; (defun foo (from to)
;;   (interactive "r")
;;   (overlay-put (make-overlay from to) 'invisible t))

;; (add-to-invisibility-spec '(work-log . t))

;;   (let ((o (make-overlay from to)))
;;     (overlay-put o 'invisible 'work-log))

;; (defvar work-log-mode-map
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m (kbd "C-c i") 'work-log-new-entry)
;;     m)
;;   "Keymap for Work Log major mode.")

;; (defcustom work-log-mode-hook nil
;;   "Normal hook run by `work-log-mode'."
;;   :type 'hook
;;   :group 'work-log)

;; (defvar work-log-syntax-table
;;   (let ((x (make-syntax-table)))
;;     (modify-syntax-entry ?# "<" x)
;;     (modify-syntax-entry ?\n ">" table)
;;     x))
