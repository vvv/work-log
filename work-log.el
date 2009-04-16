(defvar work-log-font-lock-keywords nil "XXX")
(setq work-log-font-lock-keywords
      '(("^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" . font-lock-string-face)
	("^[[:space:]]*#.*$" . font-lock-comment-face)))

;;;###autoload
(define-derived-mode work-log-mode text-mode "Work-Log"
  "XXX
Format was inspired by John Carmak's
<http://doom-ed.com/blog/1997/10/10/last-two-months-of-work-log>:

  * entry was completed on that day
  + entry was completed on a later day
  - entry was decided against on a later day

\\{work-log-mode-map}"
  (set (make-local-variable 'comment-start) "#") ; enable `M-;'
  (set (make-local-variable 'font-lock-defaults)
       '(work-log-font-lock-keywords t nil nil nil)))

(provide 'work-log)

; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
; (autoload 'work-log-mode "~/src/work-log/work-log.el" "XXX" t)
; (unload-feature 'work-log)

;; (defcustom work-log-mode-hook nil
;;   "Normal hook run by `work-log-mode'."
;;   :type 'hook
;;   :group 'work-log)

;; (defvar work-log-syntax-table
;;   (let ((x (make-syntax-table)))
;;     (modify-syntax-entry ?# "<" x)
;;     (modify-syntax-entry ?\n ">" table)
;;     x))
