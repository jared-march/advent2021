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

(defun bit-vector-integer (bit-vector)
  (reduce (lambda (a b) (+ (* 2 a) b)) bit-vector))

(defun count-binary-places (digits-list)
  (loop
    with digit-counts = (make-array (length (car digits-list)))
    with input-length = (length digits-list)
    for binary-digits in digits-list
    do (loop for i from 0 below (length binary-digits)
	     do (incf (aref digit-counts i) (aref binary-digits i)))
    finally (return digit-counts)))

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
