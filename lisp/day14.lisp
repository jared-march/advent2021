;;;; Advent of Code 2021 Day 14

;;; I'm not certain that it is possible to get a pair that doesn't
;;; have a transformation!
;;; Anyway, we have a string, a sequence of characters.
;;; We also have a map from pairs of characters to a single character.
;;; We need a function that, given a string and a map,
;;; returns the string interleaved with each pair's mappings.
;;;
;;; The new array's size is 2*previous_size - 1.
;;; Start by putting the first element of the array in.
;;; We can loop from 1 to below the array size.
;;; Every iteration, put in the mapped character, then the original.

(defun ignore-char (stream char)
  (declare (ignore stream char))
  (values))

(defun read-rules (stream)
  ;; I can't believe I'm still just mucking with the readtable
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\- #'ignore-char)
    (set-macro-character #\> #'ignore-char)
    (let* ((template (symbol-name (read stream)))
           (rules (loop with out = (make-hash-table :test 'eq)
                        for left = (read stream nil)
                        for right = (read stream nil)
                        while right
                        do (setf (gethash left out) right)
                        finally (return out))))
      (values template rules))))

(defun step-polymer (polymer rules)
  (let ((new-polymer (make-array (1- (* (array-dimension polymer 0) 2))
                                 :element-type 'character)))
    (setf (aref new-polymer 0) (aref polymer 0))
    (loop for i from 1 below (array-dimension polymer 0)
          do (setf (aref new-polymer (1- (* i 2)))
                   (aref (symbol-name
                          (gethash (find-symbol (subseq polymer (1- i) (1+ i)))
                                   rules))
                         0))
          do (setf (aref new-polymer (* i 2))
                   (aref polymer i))
          finally (return new-polymer))))

(defun get-populations (polymer)
  (loop
    while (/= (array-dimension polymer 0) 0)
    collect (let ((char (aref polymer 0)))
               (cons char
                     (prog1 (count char polymer)
                       (setf polymer (remove char polymer)))))))
                       

(defun advent-14-1 ()
  (multiple-value-bind (polymer rules)
      (with-open-file (stream "../input/14/input")
        (read-rules stream))
    (loop repeat 10
          do (setf polymer (step-polymer polymer rules)))
    (let ((populations (get-populations polymer)))
      (- (loop for pop in populations maximize (cdr pop))
         (loop for pop in populations minimize (cdr pop))))))

(defun read-fast-rules (stream)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\- #'ignore-char)
    (set-macro-character #\> #'ignore-char)
    (let* ((template (loop with str = (symbol-name (read stream))
                           with hash = (make-hash-table :test 'eq)
                           for i from 1 below (array-dimension str 0)
                           do (incf (gethash (intern (subseq str (1- i) (1+ i)))
                                             hash
                                             0))
                           finally (return hash)))
           (rules (loop with out = (make-hash-table :test 'eq)
                        for left = (read stream nil)
                        for right = (read stream nil)
                        while right
                        do (setf (gethash left out) right)
                        finally (return out))))
      (loop
        with polymer = (make-hash-table :test 'eq)
        for key being each hash-key in rules
        for template-value = (gethash key template 0)
        do (setf (gethash key polymer) template-value)
        finally (return (values polymer rules))))))

(defun step-fast-polymer (polymer rules)
  (let ((new-polymer (make-hash-table :test 'eq)))
    (loop for key being each hash-key in polymer
          for value = (gethash key polymer)
          for rule = (gethash key rules)
          for key-name = (symbol-name key)
          for rule-name = (symbol-name rule)
          for left-combo = (intern (coerce (list (aref key-name 0)
                                                 (aref rule-name 0))
                                           'string))
          for right-combo = (intern (coerce (list (aref rule-name 0)
                                                  (aref key-name 1))
                                            'string))
          do (incf (gethash left-combo new-polymer 0) value)
          do (incf (gethash right-combo new-polymer 0) value)
          finally (return new-polymer))))

(defun get-elements (polymer)
  (let ((elements (make-hash-table :test 'eq)))
    (loop for key being each hash-key in polymer
          for value being each hash-value in polymer
          for key-name = (symbol-name key)
          for key-char-1 = (aref key-name 0)
          for key-char-2 = (aref key-name 1)
          do (incf (gethash key-char-1 elements 0) value)
          do (incf (gethash key-char-2 elements 0) value)
          finally (return (loop
                            for key being each hash-key in elements
                            for value being each hash-value in elements
                            if (eq key #\K)
                              do (setf value (1+ value))
                            if (eq key #\F)
                              do (setf value (1+ value))
                            do (setf (gethash key elements) (/ value 2))
                            finally (return elements))))))
             
(defun advent-14-2 ()
  (multiple-value-bind (polymer rules) (with-open-file (stream "../input/14/input")
                                         (read-fast-rules stream))
    (loop repeat 40
          do (setf polymer (step-fast-polymer polymer rules)))
    (let ((elements (get-elements polymer)))
      (loop for key being each hash-key in elements
            for value being each hash-value in elements
            do (format t "~a ~a~%" key value))
      (- (loop for value being each hash-value in elements maximize value)
         (loop for value being each hash-value in elements
               if (/= value 0)
                 minimize value)))))
