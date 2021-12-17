;;;; Advent of Code Day 16

;;; Part 1
;;;
;;; We have a hierarchy of bit-encoded packets.
;;; There is a single outermost packet.
;;; packet -> <3-bit-version> <3-bit-type>
;;; 3-bit-type = 4 -> literal
;;; literal -> (1 <4 number bits>)* 0 <4 number bits> <0 padding to align 4>
;;; 3-bit-type /= 4 -> operator
;;; operator -> <1-bit-type-id>
;;; 1-bit-type-id = 0 -> <15-bit-total-bit-length>
;;; 1-bit-type-id = 1 -> <11-bit-number-of-subpackets>


(defun binary-to-natural (bit-vector)
  (reduce (lambda (a b) (+ (* 2 a) b)) bit-vector))

(defun bit-on-p (bit)
  (= bit 1))

(defun hex-to-binary (hex-string)
  (loop
    with bit-array = (make-array (* (array-dimension hex-string 0) 4)
                                    :element-type 'bit)
    for i from 0 below (array-dimension hex-string 0)
    do (setf (subseq bit-array (* i 4) (+ (* i 4) 4))
             (ecase (aref hex-string i)
               (#\0 #*0000) (#\1 #*0001) (#\2 #*0010) (#\3 #*0011)
               (#\4 #*0100) (#\5 #*0101) (#\6 #*0110) (#\7 #*0111)
               (#\8 #*1000) (#\9 #*1001) (#\A #*1010) (#\B #*1011)
               (#\C #*1100) (#\D #*1101) (#\E #*1110) (#\F #*1111)))
    finally (return bit-array)))

(defun read-input (stream) (hex-to-binary (read-line stream nil)))

(defun type-number-to-name (number)
  (ecase number
    (0 'sum)
    (1 'product)
    (2 'minimum)
    (3 'maximum)
    (4 'literal)
    (5 'greater-than)
    (6 'less-than)
    (7 'equal-to)))

(defun length-type-bit-to-id (bit)
  (ecase bit
      (0 'total-bits-length)
      (1 'subpacket-length)))

(defun sum-versions (packet)
  (if (not (consp packet))
      0
      (+ (car packet)
         (loop for subpacket in (cddr packet)
               sum (sum-versions subpacket)))))

;; I can write fortran in any language
(defun parse-packet (bits start &aux (current start) version type-number type-name)
  (setf version (binary-to-natural (subseq bits current (+ current 3))))
  (incf current 3)
  (setf type-number (binary-to-natural (subseq bits current (+ current 3))))
  (setf type-name (type-number-to-name type-number))
  (incf current 3)
  (cond
    ((eq type-name 'literal)
     (let (numbers-remain (number 0))
       (loop
         (setf numbers-remain (bit-on-p (aref bits current)))
         (incf current 1)
         (setf number (+ (* number 16)
                         (binary-to-natural
                          (subseq bits current (+ current 4)))))
         (incf current 4)
         (when (not numbers-remain)
           (return (values current (list version type-name number)))))))
    (t
     (let (length-type-id)
       (setf length-type-id (length-type-bit-to-id (aref bits current)))
       (incf current 1)
       (ecase length-type-id
         (total-bits-length
          (let (bits-length (packets nil))
            (setf bits-length (binary-to-natural
                               (subseq bits current (+ current 15))))
            (incf current 15)
            (do ((bits-parsed 0))
                ((>= bits-parsed bits-length))
              (multiple-value-bind (new-current packet)
                  (parse-packet bits current)
                (setf packets (cons packet packets))
                (setf bits-parsed (+ bits-parsed
                                     (- new-current current)))
                (setf current new-current)))
            (values current `(,version ,type-name ,@(reverse packets)))))
         (subpacket-length
          (let (packets-length (packets nil))
            (setf packets-length (binary-to-natural
                                  (subseq bits current (+ current 11))))
            (incf current 11)
            (dotimes (i packets-length)
              (multiple-value-bind (new-current packet)
                  (parse-packet bits current)
                (setf packets (cons packet packets))
                (setf current new-current)))
            (values current `(,version ,type-name ,@(reverse packets))))))))))

(defun advent-16-1 ()
  (let ((input (with-open-file (stream "../input/16/input")
                 (read-input stream))))
    (multiple-value-bind (current packet)
        (parse-packet input 0)
      (declare (ignore current))
      (sum-versions packet))))

(defun p< (a b)
  (if (< a b) 1 0))

(defun p> (a b)
  (if (> a b) 1 0))

(defun p= (a b)
  (if (= a b) 1 0))

(defun operator-to-lisp (op)
  (ecase op
    (sum '+)
    (product '*)
    (minimum 'min)
    (maximum 'max)
    (less-than 'p<)
    (greater-than 'p>)
    (equal-to 'p=)))

(defun packet-to-lisp (packet)
  (cond
    ((numberp packet) packet)
    ((eq (cadr packet) 'literal) (caddr packet))
    (t `(,(operator-to-lisp (cadr packet))
         ,@(loop for p in (cddr packet)
                 collect (packet-to-lisp p))))))

(defun advent-16-2 ()
  (let ((input (with-open-file (stream "../input/16/input")
                 (read-input stream))))
    (multiple-value-bind (current packet)
        (parse-packet input 0)
      (declare (ignore current))
      ;; Behold my mighty power!
      (eval (packet-to-lisp packet)))))
