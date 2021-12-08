(defun read-numbers (stream)
  "Read a stream of numbers each delimited by whitespace."
  (loop
    for num = (read stream nil)
    while num
    collect num))

(defun count-increasing (list)
  "Count how many times an element in the list is larger than the previous element."
  (loop
    with prev = nil
    with out = 0
    for num in list
    do (progn (when (and prev (< prev num)) (incf out))
              (setf prev num))
    finally (return out)))

(defun sliding-window (number-list window-size)
  "Return the list with overlapping windows of window-size added together."
  (let ((windows (make-array window-size :initial-element 0)))
    (loop
      with summed-list = (list)
      for current-window = 0 then (mod (+ current-window 1) window-size)
      for num in number-list
      do (progn (map-into windows (lambda (n) (+ n num)) windows)
                (push (aref windows current-window) summed-list)
                (setf (aref windows current-window) 0))
      finally (return (nthcdr (- window-size 1) (reverse summed-list))))))


(defun advent-1-1 ()
  (let ((input-numbers (with-open-file (stream "../input/1/input")
                         (read-numbers stream))))
    (count-increasing input-numbers)))

(defun advent-1-2 ()
  (let ((input-numbers (with-open-file (stream "../input/1/input")
                         (read-numbers stream))))
    (count-increasing (sliding-window input-numbers 3))))
