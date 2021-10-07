(defun sethash (hash-table key value)
  (setf (gethash key hash-table) value))

(defun make-change (amount denominations cache)  
  (apply
   #'+
   (mapcar
    (lambda (coin)
      (let ((remain (- amount coin))
            (memo (gethash (list amount coin) cache)))
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
             (sethash
              cache
              (list amount coin)
              result))))))
    denominations)))

(let ((amount (car (read-from-string (format nil "(~a)" (read-line)))))
      (denominations (read-from-string (format nil "(~a)" (read-line))))
      (cache (make-hash-table :test #'equal)))
  (format t "~a" (make-change amount denominations cache)))
