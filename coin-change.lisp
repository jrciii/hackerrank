(defun make-change (amount denominations cache)  
  (apply
   #'+
   (mapcar
    (lambda (coin)
      (let ((remain (- amount coin))
            (memo (aref cache amount coin)))
	(cond
	  (memo memo)
	  ((< remain 0) 0)
	  ((= remain 0) 1)
	  (t
	   (let ((result
		   (make-change
		    remain
		    (remove-if (lambda (d) (< d coin)) denominations)
		    cache)))
             (setf (aref cache amount coin) result))))))
    denominations)))

(let* ((amount (car (read-from-string (format nil "(~a)" (read-line)))))
       (denominations (read-from-string (format nil "(~a)" (read-line))))
       (cache (make-array (list (1+ amount) (1+ (apply #'max denominations))) :initial-element nil)))
  (format t "~a" (make-change amount denominations cache)))
