(defun new-force-tick-message (eh causing-message)
  (new-message "." (new-datum-tick) (new-cause eh causing-message)))
  
(defun input-not-empty (eh)
  (not (fifo-empty (Eh-input eh))))
(defun input-pop (eh)
  (fifo-pop (Eh-input eh)))

(defun output-not-empty (eh)
  (not (fifo-empty (Eh-output eh))))
(defun output-pop (eh)
  (fifo-pop (Eh-output eh)))

(defun is-idle (eh)
  (eq 'idle (Eh-state eh)))

(defun is-active (eh)
  (eq 'active (Eh-state eh)))

(defun force-idle (eh)
  (set (Eh-state eh) 'idle))
(defun force-active (eh)
  (set (Eh-state eh) 'active))


(defun sender-matches (connection fname from message)
  (let ((from-sender (make-Sender fname from (Message-port message)))) ;; this is optimizable - we should create from-sender only once, not every time (like here)
    (sender-eq from-sender (Connection-sender connection))))

(defun ensure-message-was-sent (was-sent container from message)
  (unless was-sent
    (format error-output "~%~%*** Error: ***")
    (format error-output "~%*** message from ~a dropped on floor message=~a" from message)
    (format error-output "~%*** possible connections ***")
    (mapc #'(lambda (c)
	      (format error-output "~%  ~a" c))
	  (Eh-connections container))
    (assert nil)))

(defun set-active (eh)
  (setf (Eh-state eh) 'active))
(defun set-idle (eh)
  (setf (Eh-state eh) 'idle))

(defun print-output-list (eh)
  (format *standard-error* "~%~a" (Eh-output eh)))

(defun print-specific-output (eh port)
  (mapc #'(lambda (msg)
	    (when (equal port (Message-port msg))
	      (format *standard-error* "~%~a" msg)
	      (return-from print-specific-output)))
	(Eh-output eh))
  (format *standard-output* "~%~a***No Output Message Found with port=~a" port))


(define-symbol-macro _owners-name
    (Eh-name owner))
