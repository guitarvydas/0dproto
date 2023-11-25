(defun process (name-prefix name owner)
? ? ?

(defun probe (given-name owner)
  (let ((name (format nil "~a.~a" _owners-name given-name)))
    (let ((handler 
	   (lambda (self msg)
	     (let ((s (Datum-repr (Message-datum msg))))
	       (format *error-output* "~a~%" s)))))
      (new-leaf name owner nil handler))))
	
