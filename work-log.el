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

(defvar work-log-font-lock-keywords nil "XXX")
(setq work-log-font-lock-keywords
      '(("^[[:blank:]]*#.*$" . 'font-lock-comment-face)
	("^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" . 'font-lock-string-face)
	("^[[:blank:]]+[+*] .*$" . 'work-log-completed)
	("^[[:blank:]]+- .*$" . 'work-log-decided-against)))

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

(provide 'work-log)

;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;; (defcustom work-log-mode-hook nil
;;   "Normal hook run by `work-log-mode'."
;;   :type 'hook
;;   :group 'work-log)

;; (defvar work-log-syntax-table
;;   (let ((x (make-syntax-table)))
;;     (modify-syntax-entry ?# "<" x)
;;     (modify-syntax-entry ?\n ">" table)
;;     x))
