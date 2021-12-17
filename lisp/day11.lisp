;;;; Advent of Code Day 11

;;; We have a 2D array of fixnums that init to 0 <= num < 10.
;;; We need to do steps, where:
;;; 1. Increase each number by 1.
;;; 2. While there is a number on the board >= 9:
;;;    - Set that number to most-negative-fixnum.
;;;    - Increase all adjacent (and diagonal) numbers by 1.
;;; 3. Set all negative numbers to 0.

(defun read-octopuses (stream)
  (let ((lines (loop for line = (read-line stream nil)
                     while line
                     collect (loop for char across line
                                   collect (digit-char-p char)))))
    (make-array `(,(length lines) ,(length (car lines)))
                :element-type 'fixnum
                :initial-contents lines)))

(defun safe-array-inc (array y x)
  (destructuring-bind (ydim xdim) (array-dimensions array)
    (when (and (<= 0 y) (< y ydim)
               (<= 0 x) (< x xdim))
      (incf (aref array y x)))))

(defun flash (array y x)
  (loop for yt from (1- y) to (1+ y)
        do (loop for xt from (1- x) to (1+ x)
                 do (safe-array-inc array yt xt)))
  (setf (aref array y x) most-negative-fixnum))
        
(defun advent-11-1 ()
  (let ((grid (with-open-file (stream "../input/11/input")
                (read-octopuses stream))))
    (loop
      with flashes = 0
      with (ydim xdim) = (array-dimensions grid)
      for i below 100
      do (loop for y from 0 below ydim
               do (loop for x from 0 below xdim
                        do (incf (aref grid y x))))
      do (loop
           with flash-found = t
           while flash-found
           do (progn (setf flash-found nil)
                     (loop
                       for y from 0 below ydim
                       do (loop for x from 0 below xdim
                                do (when (< 9 (aref grid y x))
                                     (setf flash-found t)
                                     (incf flashes)
                                     (flash grid y x))))))
      do (loop
           for y from 0 below ydim
           do (loop for x from 0 below xdim
                    do (setf (aref grid y x) (max 0 (aref grid y x)))))
      finally (return flashes))))

(defun advent-11-2 ()
  (let ((grid (with-open-file (stream "../input/11/input")
                (read-octopuses stream))))
    (loop
      with (ydim xdim) = (array-dimensions grid)
      for flashes = 0
      for generation = 1 then (1+ generation)
      do (loop for y from 0 below ydim
               do (loop for x from 0 below xdim
                        do (incf (aref grid y x))))
      do (loop
           with flash-found = t
           while flash-found
           do (progn (setf flash-found nil)
                     (loop
                       for y from 0 below ydim
                       do (loop for x from 0 below xdim
                                do (when (< 9 (aref grid y x))
                                     (setf flash-found t)
                                     (incf flashes)
                                     (flash grid y x))))))
      do (loop
           for y from 0 below ydim
           do (loop for x from 0 below xdim
                    do (setf (aref grid y x) (max 0 (aref grid y x)))))
      if (= flashes (* ydim xdim))
        do (return generation))))
          
