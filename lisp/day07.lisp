(defun ignore-commas (stream char)
  (declare (ignore stream char))
  (values))

(defun read-numbers (stream)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, #'ignore-commas)
    (loop
      for num = (read stream nil)
      while num
      collect num)))

(defun difference (number-1 number-2)
  (abs (- number-1 number-2)))

(defun increasing-difference (number-1 number-2)
  (let ((n (abs (- number-1 number-2))))
    (/ (* n (+ n 1)) 2)))

(defun advent-7-1 ()
  (let* ((positions (with-open-file (stream "../input/7/input")
                      (read-numbers stream)))
         (range-min (loop with min = nil
                          for position in positions
                          when (or (null min) (< position min))
                            do (setf min position)
                          finally (return min)))
         (range-max (loop with max = nil
                          for position in positions
                          when (or (null max) (< max position))
                            do (setf max position)
                          finally (return max))))
    (reduce #'min
            (loop
              for i from range-min to range-max
              collect (loop
                        with out = 0
                        for position in positions
                        do (incf out (difference position i))
                           finally (return out))))))

(defun advent-7-2 ()
  (let* ((positions (with-open-file (stream "../input/7/input")
                      (read-numbers stream)))
         (range-min (loop with min = nil
                          for position in positions
                          when (or (null min) (< position min))
                            do (setf min position)
                          finally (return min)))
         (range-max (loop with max = nil
                          for position in positions
                          when (or (null max) (< max position))
                            do (setf max position)
                          finally (return max))))
    (reduce #'min
            (loop
              for i from range-min to range-max
              collect (loop
                        with out = 0
                        for position in positions
                        do (incf out (increasing-difference position i))
                           finally (return out))))))
