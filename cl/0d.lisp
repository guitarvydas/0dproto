;; Eh-states: 'idle 'active
;; Eh: struct (name input output owner children connections handler instance-data state kind)
;; Eh.kind: 'container 'leaf
;; Eh.children: list of Eh
;; Eh.connections: list of Connector

(defstruct (Eh 
	     (:copier copy-eh))
  (name "")
  (input (make-FIFO))
  (output (make-FIFO))
  (owner nil)
  (children ())
  (connections ())
  (handler (lambda (eh msg) (assert nil)))
  (instance-data nil)
  (state 'idle)
  (kind 'unknown-kind))

(defun new-container (name owner)
  (make-Eh
   :name (format nil "~a.~a" owners-name name)
   :owner owner
   :handler container-handler
   :kind 'container))

(defun new-leaf (name owner instance-data handler)
  (make-Eh
   :name (format nil "~a.~a" (owners-name owner) name)
   :owner owner
   :handler handler
   :instance-data instance-data
   :kind 'leaf))

(defun send (eh port datum causing-message)
  (let ((cause (new-cause eh causing-message)))
    (let ((m (new-message port datum cause)))
      (fifo-push (eh-output eh) m))))

(defun send-string (eh port s causing-message)
  (let ((d (new-datum-string s)))
    (send eh port d causing-message)))

(defun forward (eh port msg)
  (fifo-push (eh-output eh) msg))


(defun container-handler (eh msg)
  (route eh nil msg)
  (when (any-child-ready eh)
    (step-children eh msg)))

(defun destroy-container (eh) )

(defun step-children (container causing-message)
  (force-idle container)
  (let ((ok nil)
	msg)
    (mapc #'(lambda (child)
	      (progn
		(if (input-not-empty child)
		    (progn
		      (setf ok t)
		      (setf msg (input-pop child)))
		    (if (is-idle child)
			(setf ok t)
			(setf msg (new-force-tick-message child causing-message))))
		(when ok
		    (progn
		      (invoke-handler child msg)
		      (destroy-message msg)))
		(when (is-active child)
		  (force-active container))
		(when (output-not-empty child)
		  (let ((outm (output-pop child)))
		    (route container child outm)
		    (destroy-message outm)))))
	  (Eh-children container))))


;; routing
(defun route (container from message)
  (let ((was-sent nil))
    (if (is-tick-message message)
	(progn
	  (mapc #'(lambda (child)
		    (attempt-tick child))
		(Eh-children container))
	  (setf was-sent t))
	(progn
	  (let ((fname
		 (if (not (null from))
		     (get-name from)
		     "")))
	    (mapc #'(lambda (connection)
		      (when (sender-matches connection fname from (Message-port message))
			(deposit connection message)
			(setf was-sent t)))
		  (Eh-connections container)))))
    (ensure-message-was-sent was-sent container from message)))

(defun deposit (c message)
  (let ((new-message (clone-message message)))
    (let ((rcvr (Connector-receiver c)))
      (setf (Message-port new-message) (Receiver-port rcvr))
      (fifo-push (Receiver-queue rcvr) new-message))))

(defun any-child-ready (container)
  (mapc #'(lambda (child)
	    (when (is-ready child)
	      (return-from any-child-ready t)))
	(Eh-children container))
  nil)

(defun is-ready (eh)
  (or 
   (output-not-empty eh)
   (input-not-empty eh)
   (not (is-idle eh))
   (any-child-ready eh)))


