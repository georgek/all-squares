(in-package :gk-all-squares)

;;;; data points are labelled simply from 0 to m-1 where |dset|=m
;;;; the initial matrix contains the distances between each pair of data points
;;;; (we only count the distance for each pair once)
;;;; then the initial clustering is {{0},{1},...,{m-1}} and the values (i,j) in
;;;; the matrix is the cost when merging clusters i and j together

;;; makes a distance matrix and priority queue from dataset
(defun make-matrix (dset metric)
  (let ((hashmap (make-hash-table :test 'equal)))
    (loop for i from 0 to (1- (length dset)) do
         (loop for j from (1+ i) to (1- (length dset)) do
              (setf (gethash (cons (list i) (list j)) hashmap)
                    (squared (funcall metric (elt dset i) (elt dset j))))))
    hashmap))

;;; convenience function for getting distances from hashmap
(defun mat-dist (hashmap x y)
  (if (eq x y)
      0
      (if (gethash (cons x y) hashmap)
          (gethash (cons x y) hashmap)
          (gethash (cons y x) hashmap))))

;;; turns the n-clustering (original dataset) into a k-clustering by merging
(defun greedy-agglom (hashmap n)
  (let* ((cur-lev (loop for i from 0 to (1- n) collecting (list i)))
         (hier (list cur-lev)))
    (loop ;;repeat 1 do
       while (> (length cur-lev) 1) do

       ;; find min merge
         (let* ((merging (loop with m = (cons (car cur-lev) (cadr cur-lev))
                            for i on cur-lev do
                            (loop for j in (rest i) do
                                 (when (< (gethash (cons (car i) j) hashmap)
                                          (gethash m hashmap))
                                   (setf m (cons (car i) j))))
                            finally (return m)))
                (merged (append (car merging) (cdr merging))))
           ;; calculate new distances
           (loop with mdist = (gethash merging hashmap)
              for i in cur-lev do
              (setf (gethash (cons merged i) hashmap)
                    (+ mdist
                       (mat-dist hashmap (car merging) i)
                       (mat-dist hashmap (cdr merging) i))))
           ;; remove merged from clustering
           (if (equal (car cur-lev) (car merging))
               (setf cur-lev (cdr cur-lev))
               (loop for i on cur-lev do
                    (when (endp (cdr i))
                      (return))
                    (when (equal (cadr i) (car merging))
                      (setf (cdr i) (cddr i))
                      (return))))
           (if (equal (car cur-lev) (cdr merging))
               (setf cur-lev (cdr cur-lev))
               (loop for i on cur-lev do
                    (when (endp (cdr i))
                      (return))
                    (when (equal (cadr i) (cdr merging))
                      (setf (cdr i) (cddr i))
                      (return))))
           (setf cur-lev (cons merged cur-lev))
           (setf hier (cons (copy-tree cur-lev) hier))))
    hier))
