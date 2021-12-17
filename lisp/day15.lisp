;;;; Advent of Code 2021 Day 15

;;; They get harder from here!
;;; We have a grid of numbers, and we need to find the cheapest path from
;;; the top left to the bottom right.
;;; We can just use Dijkstra's algorithm for this.

(defun read-grid (stream)
  (let* ((lines (loop for line = (read-line stream nil)
                      while line
                      collect (loop for char across line
                                    collect (digit-char-p char))))
        (array (make-array `(,(length lines)
                             ,(length (car lines)))
                           :element-type 'fixnum
                           :initial-contents lines)))
    array))

(defun print-grid (grid)
  (loop for y from 0 below (array-dimension grid 0)
        do (loop for x from 0 below (array-dimension grid 1)
                 do (princ (aref grid y x)))
        do (terpri))
  (terpri))

(defun safe-costs-aref (grid y x)
  (if (and (<= 0 y) (< y (array-dimension grid 0))
           (<= 0 x) (< x (array-dimension grid 1)))
      (aref grid y x)
      most-positive-fixnum))

(defun safe-visited-aref (grid y x)
  (if (and (<= 0 y) (< y (array-dimension grid 0))
           (<= 0 x) (< x (array-dimension grid 1)))
      (aref grid y x)
      1))

(defun smallest-unvisited (visited path-costs)
  (destructuring-bind (ydim xdim) (array-dimensions visited)
    (loop with out-y = nil
          with out-x = nil
          for y from 0 below ydim
          do (loop for x from 0 below xdim
                   do (when (and (= 0 (aref visited y x))
                                 (or (null out-y)
                                     (< (aref path-costs y x)
                                        (aref path-costs out-y out-x))))
                        (setf out-y y)
                        (setf out-x x)))
          finally (return `(,out-y ,out-x)))))
                        
;;; Amazingly, this is good enough for the big case in 20 mins or so.
(defun find-path (costs)
  (let* ((ydim (array-dimension costs 0))
         (xdim (array-dimension costs 1))
         (path-costs (make-array (array-dimensions costs)
                           :element-type 'fixnum
                           :initial-element most-positive-fixnum))
         (visited (make-array (array-dimensions costs)
                              :element-type 'bit
                              :initial-element 0))
         (previous (make-array (list ydim xdim 2)
                               :initial-element 0))
         (end-y (1- ydim))
         (end-x (1- xdim)))
    (setf (aref path-costs 0 0) 0)
    (loop
      for (current-y current-x) = (smallest-unvisited visited path-costs)
      while (= 0 (aref visited end-y end-x))
      do (loop
           for (y x) in (list (list (1- current-y) current-x)
                              (list (1+ current-y) current-x)
                              (list current-y (1- current-x))
                              (list current-y (1+ current-x)))
           for tentative-cost = (+ (aref path-costs current-y current-x)
                                   (safe-costs-aref costs y x))
           when (and (= 0 (safe-visited-aref visited y x))
                     (< tentative-cost (aref path-costs y x)))
             do (progn
                  (setf (aref path-costs y x) tentative-cost)
                  (setf (aref previous y x 0) current-y)
                  (setf (aref previous y x 1) current-x)))
      do (setf (aref visited current-y current-x) 1)
      finally (return (aref path-costs end-y end-x)))))
                          

(defun advent-15-1 ()
  (let ((grid (with-open-file (stream "../input/15/input")
                (read-grid stream))))
    (find-path grid)))

(defun embiggen (grid)
  (let* ((old-y (array-dimension grid 0))
         (old-x (array-dimension grid 1))
         (new-grid (make-array (list (* 5 old-y)
                                     (* 5 old-x))
                               :element-type 'fixnum)))
    (loop
      for ymod from 0 below 5
      do (loop
           for xmod from 0 below 5
           do (loop
                for y from 0 below old-y
                do (loop
                     for x from 0 below old-x
                     do (let ((val (+ ymod xmod (aref grid y x))))
                          (when (< 9 val)
                            (setf val (- val 9)))
                          (setf (aref new-grid
                                      (+ (* ymod old-y) y)
                                      (+ (* xmod old-x) x))
                                val))))))
    new-grid))

(defun advent-15-2 ()
  (let ((grid (with-open-file (stream "../input/15/input")
                (read-grid stream))))
    (find-path (embiggen grid))))
