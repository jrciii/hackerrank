(defvar amt (car (read-from-string (format nil "(~a)" (read-line)))))
(defvar denoms (read-from-string (format nil "(~a)" (read-line))))

(defvar *c* (make-hash-table :test #'equal))

(defun seth (h k v)
  (setf (gethash k h) v))

(defun make-change (amount values)  
  (apply #'+ (mapcar
	      (lambda (coin)
		(let ((remain (- amount coin))
		      (memo (gethash (list amount coin) *c*)))
		  (cond
		    (memo memo)
		    (t
		     (cond
		       ((< remain 0) 0)
		       ((= remain 0) 1)
		       (t
			(let ((result (make-change remain (remove-if (lambda (x) (< x coin)) values))))
			  (seth *c* (list amount coin) result))))))))
	      values)))

(setf *c* (make-hash-table :test #'equal))

(format t "~a" (make-change amt denoms))
