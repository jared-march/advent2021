;;;; Advent of Code Day 13

;;; We have a 2D bitarray.
;;; Dots are read into the array via X, Y coordinates.
;;; We are asked to "fold" the array.

;;; Looking at the input, the folds are always exactly on the middle of the sheet.
;;; The folded array, if folded on y, will be:
;;; old-array (y x)
;;; new-array (floor(y/2), x)
;;; for all y, x in new-array:
;;; filled if old-array(y, x) or old-array(ydim - 1 - y, x) is filled

(defun ignore-char (stream char)
  (declare (ignore stream char))
  (values))

(defun read-instructions (stream)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, #'ignore-char)
    (set-macro-character #\= #'ignore-char)
    (let* ((coordinates (loop
                          for line = (read-line stream nil)
                          while (and line
                                     (< 0 (array-dimension line 0))
                                     (digit-char-p (aref line 0)))
                          collect (with-input-from-string (stream line)
                                    (list (read stream) (read stream)))))
           (flips (loop
                    for line = (read-line stream nil)
                    while line
                    collect (with-input-from-string (stream line)
                              (read stream)
                              (read stream)
                              (cons (read stream) (read stream))))) ; direction, width
           (array (make-array `(,(+ 1 (* 2 (cdr (assoc 'y flips))))
                                ,(+ 1 (* 2 (cdr (assoc 'x flips)))))
                              :element-type 'bit
                              :initial-element 0)))
      (loop for (x y) in coordinates
            do (setf (aref array y x) 1))
      (values array flips))))

(defun flip-y (array)
  (destructuring-bind (ydim xdim) (array-dimensions array)
    (let* ((new-ydim (floor (/ ydim 2)))
           (new-xdim xdim)
           (new-array (make-array `(,new-ydim
                                    ,new-xdim)
                                  :element-type 'bit
                                  :initial-element 0)))
      (loop for y from 0 below new-ydim
            do (loop for x from 0 below new-xdim
                     do (setf (aref new-array y x)
                              (max (aref array y x)
                                   (aref array (- ydim 1 y) x)))))
      new-array)))

(defun flip-x (array)
  (destructuring-bind (ydim xdim) (array-dimensions array)
    (let* ((new-ydim ydim)
           (new-xdim (floor (/ xdim 2)))
           (new-array (make-array `(,new-ydim
                                    ,new-xdim)
                                  :element-type 'bit
                                  :initial-element 0)))
      (loop for y from 0 below new-ydim
            do (loop for x from 0 below new-xdim
                     do (setf (aref new-array y x)
                              (max (aref array y x)
                                   (aref array y (- xdim 1 x))))))
      new-array)))

(defun print-board (array)
  (destructuring-bind (ydim xdim) (array-dimensions array)
    (loop for y from 0 below ydim
          do (loop for x from 0 below xdim
                   do (if (= 0 (aref array y x))
                          (princ #\space)
                          (princ #\#)))
          do (terpri))
    (terpri)))

(defun advent-13-1 ()
  (multiple-value-bind (array flips)
      (with-open-file (stream "../input/13/input")
        (read-instructions stream))
    (if (eq 'x (caar flips))
        (setf array (flip-x array))
        (setf array (flip-y array)))
    (destructuring-bind (ydim xdim) (array-dimensions array)
      (loop for y from 0 below ydim
            sum (loop for x from 0 below xdim
                      sum (aref array y x))))))


(defun advent-13-2 ()
  (multiple-value-bind (array flips)
      (with-open-file (stream "../input/13/input")
        (read-instructions stream))
    (loop for flip in flips
          if (eq (car flip) 'x)
            do (setf array (flip-x array))
          if (eq (car flip) 'y)
            do (setf array (flip-y array)))
    (print-board array)))
