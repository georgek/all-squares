;;; for debugging
(defmacro view (variable)
  `(format t "~s: ~a~%" ',variable ,variable))
