(in-package :gk-all-squares)

;;;; heap for doing greedy clustering
(defclass queue ()
  ((comparison
    :initarg :comparison
    :initform #'>
    :documentation "A binary comparison operator which should return true if
    the LHS has higher priority than the RHS.")
   (array
     :initform (make-array 10 :fill-pointer 0 :adjustable t))))

(defun enqueue (queue element &optional (priority element))
  (with-slots (comparison array) queue
    (let ((ind (vector-push-extend (cons priority element) array)))
      (loop while (and (> ind 0)
                       (funcall comparison 
                                (car (elt array ind)) 
                                (car (elt array (pa ind)))))
         do
         ;; swap with parent
           (rotatef (elt array ind)
                    (elt array (pa ind)))
           (setf ind (pa ind))))))

(defun dequeue (queue)
  (with-slots (comparison array) queue
    (when (= (length array) 0)
      (return-from dequeue nil))
    (when (= (length array) 1)
      (return-from dequeue (cdr (vector-pop array))))
    (let ((top (elt array 0)))
      (setf (elt array 0) (vector-pop array))
      (loop with ind = 0 and largest = 0 do
           (when (and (< (fc ind) (length array))
                      (funcall comparison
                               (car (elt array (fc ind)))
                               (car (elt array ind))))
             (setf largest (fc ind)))
           (when (and (< (sc ind) (length array))
                      (funcall comparison
                               (car (elt array (sc ind)))
                               (car (elt array largest))))
             (setf largest (sc ind)))
           (if (not (= ind largest))
               (progn
                 (rotatef (elt array ind)
                          (elt array largest))
                 (setf ind largest))
               (return)))
      (cdr top))))

(defun peek (queue)
  (with-slots (array) queue
    (if (= (length array) 0)
        nil
        (elt array 0))))

(defun empty-queue (queue)
  (with-slots (array) queue
    (let ((oldarr array))
      (setf array (make-array 10 :fill-pointer 0 :adjustable t))
      oldarr)))

(defun queue-size (queue)
  (with-slots (array) queue
    (length array)))

;;; indexing functions
(defun fc (index)
  "Returns index of first child."
  (+ (* index 2) 1))

(defun sc (index)
  "Returns index of second child."
  (+ (* index 2) 2))

(defun pa (index)
  "Returns index of parent."
  (floor (/ (- index 1) 2)))

;;; default print for queue
(defmethod print-object ((object queue) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (array) object
      (format stream "~s" array))))
