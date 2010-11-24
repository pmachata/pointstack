;;;
;;;
;;;

(defvar pointstack-stack '() "Stack of markers")

(defun pointstack-update ()
  (interactive "")
  (save-excursion
    (let ((buffer (get-buffer-create "*pointstack*"))
	  (ptstk pointstack-stack))
      (display-buffer buffer)
      (set-buffer buffer)
      (delete-region (point-min) (point-max))
      (while ptstk
	(let* ((marker (car ptstk))
	       (buffer (marker-buffer marker))
	       (snipbeg 0)
	       (snipend 0)
	       (lines 0))
	  (save-excursion
	    (set-buffer buffer)
	    (goto-char (marker-position marker))
	    (setq snipbeg (line-beginning-position 0))
	    (setq snipend (line-end-position 2))
	    (setq lines (count-lines
			 (point-min)
			 (line-end-position))))
	  (insert-string (buffer-name buffer) ":" lines "\n")
	  (insert-buffer-substring buffer snipbeg snipend)
	  (insert-string "\n\n")
	  (goto-char (point-max))
	  (setq ptstk (cdr ptstk)))))))


(defun pointstack-do-move (message marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (message (format "%s %s" message marker))
  (pointstack-update))

(defun pointstack-do-push (message marker)
  (setq pointstack-stack (cons marker pointstack-stack))
  (message (format "Push %s" marker))
  (pointstack-update))


(defun pointstack-current-marker ()
  (set-marker (make-marker) (point) (current-buffer)))

(defun pointstack-push ()
  (interactive "")
  (pointstack-do-push "Push" (pointstack-current-marker)))

(defun pointstack-swap-point-and-top ()
  (interactive "")
  (let ((new-marker (pointstack-current-marker)))
    (pointstack-pop)
    (pointstack-do-push "Swap" new-marker)))

(defun pointstack-pop ()
  (interactive "")
  (if pointstack-stack
      (pointstack-do-move "Pop" (pop pointstack-stack))
    (message (format "Pointstack is empty" pointstack-stack))))

(defun pointstack-top ()
  (interactive "")
  (if pointstack-stack
      (pointstack-do-move "Top" (car pointstack-stack))
    (message (format "Pointstack is empty" pointstack-stack))))

(defun pointstack-discard-pop ()
  (interactive "")
  (if pointstack-stack
      (pop pointstack-stack)
    (message (format "Pointstack is empty" pointstack-stack))))

(defun pointstack-clear ()
  (interactive "")
  (setq pointstack-stack '())
  (pointstack-update))

(defun pointstack-debug ()
  (interactive "")
  (message (format "pointstack='%s'" pointstack-stack)))

