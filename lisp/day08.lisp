;;;; Advent of Code 2021 Day 8

;;; Part 2:
;;; We have a relation from decoded values to decoded digits:
;;; Decoded values: 1  2     3     4    5     6      7   8       9      0
;;; Decoded digits: cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg abcefg
;;; The digits are sets of segments that are turned on.
;;;
;;; For each line of input, we have a set of encoded digits and a sequence of output encoded digits.
;;; There is a one-to-one relation between:
;;; - encoded segments and decoded segments;
;;; - encoded digits and decoded digits;
;;; - and encoded values and decoded values.
;;; The relation between encoded digits and decoded digits is given by:
;;; e1 .. en R d1 .. dn if decode(ei) = di for all i | 0 <= i <= n.
;;;
;;; Observing the length of each digit,
;;; value(d) = 1  iff  length(d) = 2
;;; value(d) = 7  iff  length(d) = 3
;;; value(d) = 4  iff  length(d) = 4
;;; value(d) = 8  iff  length(d) = 7
;;; length(d) = 5  iff  value(d) is in {2, 3, 5}
;;; length(d) = 6  iff  value(d) is in {6, 9, 0}
;;; value(d) = 0  iff  length(d) = 6  and
;;;                    digit(4) - digit(1) - d != {}
;;; value(d) = 6  iff  length(d) = 6  and
;;;                    digit(1) - d != {}
;;; value(d) = 9  iff  length(d) = 6
;;;                    value(d) not in {0, 6}
;;; value(d) = 5  iff  length(d) = 5  and
;;;                    d - digit(6) = {}
;;; value(d) = 3  iff  length(d) = 5  and
;;;                    value(d) != 5  and
;;;                    d - digit(9) = {}
;;; value(d) = 2  iff  length(d) = 5  and
;;;                    value(d) not in {3, 5}

;;; This really threw me for a loop for some reason. 🤷

(defun set-equal (list-1 list-2)
  (and (subsetp list-1 list-2) (subsetp list-2 list-1)))

(defun pipe-symbol (stream char)
  (declare (ignore stream char))
  'pipe)

(defun read-notes (stream)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\| #'pipe-symbol)
    (loop for line = (read-line stream nil)
          while line
          collect (with-input-from-string (stream line)
                    (let* ((patterns (loop for token = (read stream nil)
                                           until (eq token 'pipe)
                                           collect (loop for c across (symbol-name token)
                                                         collect c)))
                           (output-digits (loop for token = (read stream nil)
                                                while token
                                                collect (loop for c across (symbol-name token)
                                                              collect c))))
                      (list patterns output-digits))))))

(defun digit-1478-p (symbol)
  (let ((segments (symbol-name symbol)))
    (case (length segments)
      ((2 3 4 7) t))))

(defun advent-8-1 ()
  (let ((notes (with-open-file (stream "../input/8/input")
                 (read-notes stream))))
    (loop
      with out = 0
      for (patterns output-digits) in notes
      do (loop for digit in output-digits
               if (digit-1478-p digit)
                 do (incf out))
      finally (return out))))
      
(defparameter *decoded-values-digits*
  '((0 . (#\a #\b #\c   #\e #\f #\g))
    (1 . (    #\c     #\f  ))
    (2 . (#\a   #\c #\d #\e   #\g))
    (3 . (#\a   #\c #\d   #\f #\g))
    (4 . (  #\b #\c #\d   #\f  ))
    (5 . (#\a #\b   #\d   #\f #\g))
    (6 . (#\a #\b   #\d #\e #\f #\g))
    (7 . (#\a   #\c     #\f  ))
    (8 . (#\a #\b #\c #\d #\e #\f #\g))
    (9 . (#\a #\b #\c #\d   #\f #\g)))
  "An alist of values and their associated segments.")

(defun advent-8-2 ()
  (let ((notes (with-open-file (stream "../input/8/input")
                 (read-notes stream))))
    (loop
      for (encoded-digits encoded-output-digits) in notes
      sum (let* ((encoded-values-digits (loop for i in encoded-digits
                                              collect (cons nil i))))
            (rplaca (rassoc-if (lambda (x) (= 2 (length x))) encoded-values-digits)
                    1)
            (rplaca (rassoc-if (lambda (x) (= 3 (length x))) encoded-values-digits)
                    7)
            (rplaca (rassoc-if (lambda (x) (= 4 (length x))) encoded-values-digits)
                    4)
            (rplaca (rassoc-if (lambda (x) (= 7 (length x))) encoded-values-digits)
                    8)
            (rplaca (find-if
                     (lambda (evd)
                       (let ((digit (cdr evd)))
                         (and (= 6 (length digit))
                              (set-difference
                               (set-difference (cdr (assoc 4 encoded-values-digits))
                                               (cdr (assoc 1 encoded-values-digits)))
                               digit))))
                     encoded-values-digits)
                    0)
            (rplaca (find-if
                     (lambda (evd)
                       (destructuring-bind (value &rest digit) evd
                         (and (= 6 (length digit))
                              (not (and value (= value 0)))
                              (set-difference (cdr (assoc 1 encoded-values-digits))
                                              digit))))
                     encoded-values-digits)
                    6)
            (rplaca (find-if
                     (lambda (evd)
                       (destructuring-bind (value &rest digit) evd
                         (and (= 6 (length digit))
                              (null value))))
                     encoded-values-digits)
                    9)
            (rplaca (find-if
                     (lambda (evd)
                       (let ((digit (cdr evd)))
                         (and (= 5 (length digit))
                              (null (set-difference digit
                                                    (cdr (assoc 6 encoded-values-digits)))))))
                     encoded-values-digits)
                    5)
            (rplaca (find-if
                     (lambda (evd)
                       (destructuring-bind (value &rest digit) evd
                         (and (= 5 (length digit))
                              (not (and value (= 5 value)))
                              (null (set-difference digit
                                                    (cdr (assoc 9 encoded-values-digits)))))))
                     encoded-values-digits)
                    3)
            (rplaca (find-if
                     (lambda (evd)
                       (destructuring-bind (value &rest digit) evd
                         (and (= 5 (length digit))
                              (not (and value (= value 5)))
                              (not (and value (= value 3))))))
                     encoded-values-digits)
                    2)
            (reduce (lambda (a b) (+ (* 10 a) b))
                      (mapcar
                       (lambda (eod)
                         (car (rassoc eod encoded-values-digits :test #'set-equal)))
                       encoded-output-digits))))))
