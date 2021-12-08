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

(defun read-numbers-big (stream)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, #'ignore-commas)
    (loop
      with timers = (make-array 9 :initial-element 0)
      for num = (read stream nil)
      while num
      do (incf (aref timers num))
      finally (return timers))))

(defun advent-6-1 ()
  (let ((fish-timers (with-open-file (stream "../input/6/input")
                       (read-numbers stream))))
    (loop repeat 80
          do (setf fish-timers (loop
                                 with out = (list)
                                 for timer in fish-timers
                                 if (minusp (1- timer))
                                   do (push 6 out)
                                   and do (push 8 out)
                                 else
                                   do (push (1- timer) out)
                                 finally (return out)))
          finally (return (length fish-timers)))))

(defun advent-6-2 ()
  (let ((fish-timers (with-open-file (stream "../input/6/input")
                       (read-numbers-big stream))))
    (loop repeat 256
          do (let ((tmp (aref fish-timers 0)))
               (setf (aref fish-timers 0) (aref fish-timers 1))
               (setf (aref fish-timers 1) (aref fish-timers 2))
               (setf (aref fish-timers 2) (aref fish-timers 3))
               (setf (aref fish-timers 3) (aref fish-timers 4))
               (setf (aref fish-timers 4) (aref fish-timers 5))
               (setf (aref fish-timers 5) (aref fish-timers 6))
               (setf (aref fish-timers 6) (+ (aref fish-timers 7) tmp))
               (setf (aref fish-timers 7) (aref fish-timers 8))
               (setf (aref fish-timers 8) tmp))
          finally (return (reduce #'+ fish-timers)))))
                   
