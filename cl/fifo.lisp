;; FIFOs
(defstruct (FIFO (:copier copy-fifo))
  (queue ()))

(defun fifo-push (fifo item)
  (setf (FIFO-queue fifo) (cons item (FIFO-queue fifo))))

(defun fifo-pop (fifo)
  (let ((item (last (FIFO-queue fifo))))
    (setf (FIFO-queue fifo) (butlast (FIFO-queue fifo)))))
;;

