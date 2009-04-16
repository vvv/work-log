; (autoload 'work-log-mode "~/src/work-log/work-log.el" "XXX" t)

; (setq work-log-font-lock-keywords
(defvar work-log-font-lock-keywords
  '("^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$")) ; XXX custom face

;;;###autoload
(define-derived-mode work-log-mode text-mode "Work-Log"
  "XXX
Format was inspired by John Carmak's
<http://doom-ed.com/blog/1997/10/10/last-two-months-of-work-log>:

  * entry was completed on that day
  + entry was completed on a later day
  - entry was decided against on a later day

\\{work-log-mode-map}"
;;   (modify-syntax-entry ?# "<")
  (set (make-local-variable 'font-lock-defaults)
       '(work-log-font-lock-keywords t nil nil nil))
  )

;; (defcustom work-log-mode-hook nil
;;   "Normal hook run by `work-log-mode'."
;;   :type 'hook
;;   :group 'work-log)
