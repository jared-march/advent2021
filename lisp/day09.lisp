(defun read-heightmap (stream)
  (let* ((input (loop for line = (read-line stream nil)
                      while line
                      collect (map 'list (lambda (x) (digit-char-p x))
                                   line))))
    (make-array `(,(length (car input))
                  ,(length input))
                :element-type 'fixnum
                :initial-contents input)))

(defun lowpointp (array y x)
  (destructuring-bind (ydim xdim) (array-dimensions array)
    (and (or (not (<     0     y)) (< (aref array y x) (aref array (1- y)    x )))
         (or (not (<     0     x)) (< (aref array y x) (aref array     y (1- x))))
         (or (not (< (1+ y) ydim)) (< (aref array y x) (aref array (1+ y)    x )))
         (or (not (< (1+ x) xdim)) (< (aref array y x) (aref array     y (1+ x))))
         t)))

(defun advent-9-1 ()
  (let ((heightmap (with-open-file (stream "../input/9/input")
                     (read-heightmap stream))))
    (loop
      with out = 0
      with dims = (array-dimensions heightmap)
      with rows = (car dims)
      with columns = (cadr dims)
      for row from 0 below rows
      do (loop
           for column from 0 below columns
           if (lowpointp heightmap row column)
             do (incf out (1+ (aref heightmap row column))))
      finally (return out))))

(defun basin-size-inner (array y x seen ydim xdim)
  (cond
    ((not (and (<= 0 x (1- xdim)) (<= 0 y (1- ydim)))) 0)
    ((= (aref seen y x) 1) 0)
    ((= (aref array y x) 9) 0)
    (t (setf (aref seen y x) 1)
       (+ 1
          (basin-size-inner array (1- y)    x  seen ydim xdim)
          (basin-size-inner array     y (1- x) seen ydim xdim)
          (basin-size-inner array (1+ y)    x  seen ydim xdim)
          (basin-size-inner array y     (1+ x) seen ydim xdim)))))

(defun basin-size (array y x)
  "Find the size of the basin with low point x, y."
  (let* ((seen (make-array (array-dimensions array) :element-type 'bit :initial-element 0))
         (ydim (first (array-dimensions seen)))
         (xdim (second (array-dimensions seen))))
    (basin-size-inner array y x seen ydim xdim)))
    
(defun advent-9-2 ()
  (let* ((heightmap (with-open-file (stream "../input/9/input")
                      (read-heightmap stream)))
         (basins nil))
    (loop
      with dims = (array-dimensions heightmap)
      with rows = (first dims)
      with columns = (second dims)
      for row from 0 below rows
      collect (loop
                for column from 0 below columns
                if (lowpointp heightmap row column)
                  do (push (basin-size heightmap row column) basins)))
    (setf basins (sort basins #'>))
    (* (first basins) (second basins) (third basins))))

