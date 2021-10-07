(defun make-change (amount denominations cache)  
  (apply
   #'+
   (mapcar
    (lambda (denomination)
      (let ((remain (- amount denomination))
            (memo (aref cache amount denomination)))
	(cond
	  (memo memo)
	  ((< remain 0) 0)
	  ((= remain 0) 1)
	  (t
	   (let ((result
		  (make-change
		   remain
		   (remove-if (lambda (d) (< d denomination)) denominations)
		   cache)))
             (setf (aref cache amount denomination) result))))))
    denominations)))

(let* ((amount (car (read-from-string (format nil "(~a)" (read-line)))))
       (denominations (read-from-string (format nil "(~a)" (read-line))))
       (cache (make-array (list (1+ amount) (1+ (apply #'max denominations))) :initial-element nil)))
  (format t "~a" (make-change amount denominations cache)))
