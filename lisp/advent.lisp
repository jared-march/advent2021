(defun read-binary (stream)
  "Read a stream of binary digits delimited by newlines."
  (loop
    with out = (list)
    for str = (read-line stream nil)
    while str
    do (let ((array (make-array (length str) :element-type 'bit)))
	 (loop for i from 0 below (length str)
	       do (setf (aref array i)
			(digit-char-p (aref str i))))
	 (setf out (cons array out)))
    finally (return (reverse out))))

(defun read-numbers (stream)
  "Read a stream of numbers each delimited by whitespace."
  (loop
    for num = (read stream nil)
    while num
    collect num))

(defun read-commands (stream)
  "Read a stream of commands, where commands ::= (direction magnitude #\newline)*."
  (loop
    for (direction magnitude) = (list (read stream nil) (read stream nil))
    while (and direction magnitude)
    collect (list direction magnitude)))

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

(defun advent-2-1 ()
  (loop
    with input = (with-open-file (stream "../input/2/input")
                   (read-commands stream))
    with horizontal-position = 0
    with depth = 0
    for (direction magnitude) in input
    do (ecase direction
         (forward (incf horizontal-position magnitude))
         (down (incf depth magnitude))
         (up (decf depth magnitude)))
    finally (return (* horizontal-position depth))))

(defun advent-2-2 ()
  (loop
    with input = (with-open-file (stream "../input/2/input")
                   (read-commands stream))
    with horizontal-position = 0
    with depth = 0
    with aim = 0
    for (direction magnitude) in input
    do (ecase direction
         (forward (incf horizontal-position magnitude) (incf depth (* aim magnitude)))
         (down (incf aim magnitude))
         (up (decf aim magnitude)))
    finally (return (* horizontal-position depth))))

(defun advent-3-1 ()
  (let*
      ((input (with-open-file (stream "../input/3/input")
		(read-binary stream)))
       (input-length (length input))
       (digit-counts (count-binary-places input))
       (gamma 
	 (bit-vector-integer (map 'bit-vector
				  (lambda (x) (if (< x (/ input-length 2)) 0 1))
				  digit-counts)))
       (epsilon
	 (bit-vector-integer (map 'bit-vector
				  (lambda (x) (if (>= x (/ input-length 2)) 0 1))
				  digit-counts))))
    (* epsilon gamma)))

(defun count-binary-places (digits-list)
  (loop
    with digit-counts = (make-array (length (car digits-list)))
    with input-length = (length digits-list)
    for binary-digits in digits-list
    do (loop for i from 0 below (length binary-digits)
	     do (incf (aref digit-counts i) (aref binary-digits i)))
    finally (return digit-counts)))

(defun bit-vector-integer (bit-vector)
  (reduce (lambda (a b) (+ (* 2 a) b)) bit-vector))

;; I'm sorry, I'm so sorry...
;; I need to think hard on why this happened.
(defun advent-3-2 ()
  (let*
      ((input (with-open-file (stream "../input/3/input")
		(read-binary stream)))
       (oxygen-rating
	 (loop
	   with numbers = input
	   for numbers-length = (length numbers)
	   while (/= 1 numbers-length)
	   for position from 0
	   for digit-counts = (count-binary-places numbers)
	   for position-match = (if (>= (aref digit-counts position)
					(/ numbers-length 2))
				    1
				    0)
	   do (setf numbers (remove-if (lambda (num) (/= position-match
							 (aref num position)))
				       numbers))
	   finally (return (bit-vector-integer (car numbers)))))
       (scrubber-rating
	 (loop
	   with numbers = input
	   for numbers-length = (length numbers)
	   while (/= 1 numbers-length)
	   for position from 0
	   for digit-counts = (count-binary-places numbers)
	   for position-match = (if (< (aref digit-counts position)
					(/ numbers-length 2))
				    1
				    0)
	   do (setf numbers (remove-if (lambda (num) (/= position-match
							 (aref num position)))
				       numbers))
	   finally (return (bit-vector-integer (car numbers))))))

    (* oxygen-rating scrubber-rating)))

