

;;
(defun initialize-instance Leaf :after (func ) 
(progn 
(setf (slot-value %self 'func) func ) 
(setf (slot-value %self 'reset) #'reset ) 
(setf (slot-value %self 'completed___Q) #'completed___Q ) 
(setf (slot-value %self 'handle) #'handle ) 
(setf (slot-value %self 'step) #'step ) (slot-value %self (funcall clear__outputs ))  (slot-value %self (funcall reset ))  
(setf (slot-value %self) 'armed )  ))
;;
(defun clear__outputs () (progn langjsself  «
    self.outputs = [];
  » langclself  «
    (setf (slot-value %self 'outputs) nil)
  » 
))
;;
(defun handle (message sendFunction ) (progn 
(cond ((eq (slot-value %self 'state) 'armed) (progn 
;;
(flet ((send (lambda (port data trace ) (progn (funcall push ⟨Messageport data %self trace ⟩  ) 
)))) 
(let ((val (slot-value %self (funcall func message  send  )) )) 
(progn (funcall push ⟨Message'out  val %self message ⟩  ) 
(setf (slot-value %self) 'completed ) 
)_ 
)))
(cond ((eq (slot-value %self 'state) 'completed) (progn (die "Leaf not armed"  ) 
))) 
))
;;
(defun step (sendFunction ) (progn 
(cond ((eq (slot-value %self 'state) 'completed) (progn 
)))
(cond ((eq (slot-value %self 'state) 'armed) (progn (die "internal error: Leaf/step called on armed leaf"  ) 
))) 
))
;;
(defun reset () (progn 
(setf (slot-value %self) 'armed ) 
))
;;
(defun completed___Q () (progn 
(cond ((eq (slot-value %self 'state) 'armed) (progn 'no  
)))
(cond ((eq (slot-value %self 'state) 'completed) (progn 'yes  
))) 
))
