(defun seth (h k v)
  (setf (gethash k h) v))

(defun make-change (amount values db)  
  (apply #'+ (mapcar
	      (lambda (coin)
		(let ((remain (- amount coin))
		      (memo (gethash (list amount coin) db)))
		  (cond
		    (memo memo)
		    (t
		     (cond
		       ((< remain 0) 0)
		       ((= remain 0) 1)
		       (t
			(let ((result
				(make-change
				 remain
				 (remove-if (lambda (x) (< x coin)) values)
				 db)))
			  (seth db (list amount coin) result))))))))
	      values)))

(let ((amt (car (read-from-string (format nil "(~a)" (read-line)))))
      (denoms (read-from-string (format nil "(~a)" (read-line))))
      (db (make-hash-table :test #'equal)))
  (format t "~a" (make-change amt denoms db)))
