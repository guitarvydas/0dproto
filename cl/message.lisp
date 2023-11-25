;; Message == (port datum cause)
;; Cause == (who message)
;; Port == a string

;; cloning just ensures that p is on the heap, CL puts every non-scalar item on the heap and uses a Garbage Collector to clean up the heap
(defun clone-port (p) p)

(defun make-message (port datum cause)
  (list port datum clause))

(defun clone-message (m) m)

(defun destroy-message (m) )

(defun destroy-datum (d) )

(defun destroy-port (p) )

(defun make-cause (eh msg)
  (list eh msg))
  
