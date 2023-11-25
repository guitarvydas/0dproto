;; Connectors
;; direction: 'down 'across 'up 'through
(defstruct (Connector (:copier copy-connector))
  direction
  sender
  receiver)

(defstruct (Sender (:copier copy-sender))
  name
  component
  port)

(defstruct (Receiver (:copier copy-Receiver))
  name
  queue
  port)

(defun sender-eq (s1 s2)
  (and (equal (Sender-component s1) (Sender-component s2))
       (equal (Sender-port s1)      (Sender-port s2))))

