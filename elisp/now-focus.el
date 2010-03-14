;; Note: does not work reliably in terminals!

(defun now-focus! ()
  "Open the current buffer in a frame without any bling."
  (interactive)
  ;; to restore:
  ;; (setq mode-line-format (default-value 'mode-line-format))
  (let ((frame (nowfocus-make-minimal-frame)))
    (select-frame frame)
    (setq mode-line-format nil)
    ;; for Windows, untested
    (when (fboundp 'w32-send-sys-command)
      (w32-send-sys-command 61488 frame))))

(defun nowfocus-make-minimal-frame ()
  (make-frame '((minibuffer . nil)
		(vertical-scroll-bars . nil)
		(left-fringe . 0)
		(right-fringe . 0)
		(border-width . 0)
		(internal-border-width . 64) ; whitespace!
		(cursor-type . box)
		(menu-bar-lines . 0)
		(tool-bar-lines . 0)
		(fullscreen . fullboth)
		(unsplittable . t))))

(provide 'now-focus)
