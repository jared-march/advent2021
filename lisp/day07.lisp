(defun nop (&rest ignored)
  (declare (ignore ignored))
  (values))

(defun read-numbers (stream)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, #'nop)
    (loop for num = (read stream nil) while num collect num)))

(defun difference (number-1 number-2)
  (abs (- number-1 number-2)))

(defun increasing-difference (number-1 number-2)
  (let ((n (abs (- number-1 number-2))))
    (/ (* n (+ n 1)) 2)))

(defun advent-7-1 ()
  (let* ((positions (with-open-file (stream "../input/7/input")
                      (read-numbers stream)))
         (range-min (reduce #'min positions))
         (range-max (reduce #'max positions)))
    (reduce #'min
            (loop for i from range-min to range-max
                  collect (reduce #'+ (mapcar (lambda (p) (difference p i))
                                              positions))))))

(defun advent-7-2 ()
  (let* ((positions (with-open-file (stream "../input/7/input")
                      (read-numbers stream)))
         (range-min (reduce #'min positions))
         (range-max (reduce #'max positions)))
    (reduce #'min
            (loop for i from range-min to range-max
                  collect (reduce #'+ (mapcar (lambda (p) (increasing-difference p i))
                                              positions))))))
