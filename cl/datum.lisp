(defstruct Datum
  data
  clone
  reclaim
  repr
  kind
  raw)

(defun new-datum-string (s)
  (let ((d (make-Datum)))
    (setf (data d) (copy-string s)
	  (clone d) clone-datum-string
	  (reclaim d) reclaim-datum-string
	  (repr d) repr-datum-string
	  (kind d) (lambda () "string")
	  (raw d) raw-datum-string)
    d))

(defun new-datum-bang ()
  (let ((d (make-Datum)))
    (setf (data d) t
	  (clone d) clone-datum-bang
	  (reclaim d) reclaim-datum-bang
	  (repr d) repr-datum-bang
	  (kind d) (lambda () "bang")
	  (raw d) raw-datum-bang)
    d))

(defun new-datum-tick ()
  (let ((d (make-Datum)))
    (setf (data d) t
	  (clone d) clone-datum-tick
	  (reclaim d) reclaim-datum-tick
	  (repr d) repr-datum-tick
	  (kind d) (lambda () "tick")
	  (raw d) raw-datum-tick)
    d))

(defun new-datum-bytes (b)
  (let ((d (make-Datum)))
    (setf (data d) (copy-bytes b)
	  (clone d) clone-datum-bytes
	  (reclaim d) reclaim-datum-bytes
	  (repr d) repr-datum-bytes
	  (kind d) (lambda () "bytes")
	  (raw d) raw-datum-bytes)
    d))

(defun new-datum-handle (h)
  (let ((d (make-Datum)))
    (setf (data d) (copy-handle h)
	  (clone d) clone-datum-handle
	  (reclaim d) reclaim-datum-handle
	  (repr d) repr-datum-handle
	  (kind d) (lambda () "handle")
	  (raw d) raw-datum-handle)
    d))

  
;;;;

(defun copy-handle (x) x)
(defun clone-datum-handle (d) d)
(defun reclaim-datum-handle (d) )
(defun repr-datum-handle (x) (format nil "%a" x))
(defun raw-datum-handle (x) x)

(defun clone-datum-bytes (d) d)
(defun reclaim-datum-bytes (d) )
(defun repr-datum-bytes (x) (format nil "%a" x))
(defun raw-datum-bytes (x) x)

(defun copy-tick (x) x)
(defun clone-datum-tick (d) d)
(defun reclaim-datum-tick (d) )
(defun repr-datum-tick (x) (format nil "%a" x))
(defun raw-datum-tick (x) x)

(defun copy-bang (x) x)
(defun copy-bang (x) x)
(defun clone-datum-bang (d) d)
(defun reclaim-datum-bang (d) )
(defun repr-datum-bang (x) (format nil "%a" x))
(defun raw-datum-bang (x) x)

(defun copy-string (x) x)
(defun clone-datum-string (d) d)
(defun reclaim-datum-string (d) )
(defun repr-datum-string (x) (format nil "%a" x))
(defun raw-datum-string (x) x)
