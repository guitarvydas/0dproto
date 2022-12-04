

;;
(defun initialize-instance Leaf :after (func ) 
(progn 
(setf (slot-value %self 'func) func ) (slot-value %self (funcall reset ))  
(setf (slot-value %self) 'armed )  ))
