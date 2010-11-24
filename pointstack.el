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

(defun pointstack-push ()
  (interactive "")
  (let* ((marker (set-marker (make-marker) (point) (current-buffer))))
    (setq pointstack-stack (cons marker pointstack-stack))
    (message (format "Push %s" marker))
    (pointstack-update)))

(defun pointstack-swap-point-and-top ()
  (interactive "")
  (if pointstack-stack
      (let ((new-marker (set-marker (make-marker) (point) (current-buffer)))
	    (old-marker (pop pointstack-stack)))
	(switch-to-buffer (marker-buffer old-marker))
	(goto-char (marker-position old-marker))
	(message (format "Pointstack swap %s %s" old-marker new-marker))
	(setq pointstack-stack (cons new-marker pointstack-stack))
	(pointstack-update))
    (message (format "Pointstack is empty" pointstack-stack))))

(defun pointstack-pop ()
  (interactive "")
  (if pointstack-stack
      (progn
	(let ((marker (pop pointstack-stack)))
	  (switch-to-buffer (marker-buffer marker))
	  (goto-char (marker-position marker))
	  (message (format "Pop %s" marker))
	  (pointstack-update)))
    (message (format "Pointstack is empty" pointstack-stack))))

(defun pointstack-discard-pop ()
  (interactive "")
  (if pointstack-stack
      (progn
	(message (format "Discard %s" (pop pointstack-stack)))
	(pointstack-update))
    (message (format "Pointstack is empty" pointstack-stack))))

(defun pointstack-clear ()
  (interactive "")
  (setq pointstack-stack '())
  (pointstack-update))

(defun pointstack-debug ()
  (interactive "")
  (message (format "pointstack='%s'" pointstack-stack)))

