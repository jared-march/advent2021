(defstruct line
  (x1 0)
  (y1 0)
  (x2 0)
  (y2 0))

(defun ignore-macro-character (stream char)
  (declare (ignore stream char))
  (values))

(defun read-lines (stream)
  "Return a list of line structs from stream."
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\- #'ignore-macro-character)
    (set-macro-character #\> #'ignore-macro-character)
    (set-macro-character #\, #'ignore-macro-character)
    (loop
      for line = (list (read stream nil)
                       (read stream nil)
                       (read stream nil)
                       (read stream nil))
      while (loop for i in line never (null i))
      collect (make-line :x1 (first line)
                         :y1 (second line)
                         :x2 (third line)
                         :y2 (fourth line)))))

(defun draw-line (diagram line)
  (let ((xmin (min (line-x1 line) (line-x2 line)))
        (xmax (max (line-x1 line) (line-x2 line)))
        (ymin (min (line-y1 line) (line-y2 line)))
        (ymax (max (line-y1 line) (line-y2 line))))
    (cond
      ((= xmin xmax)
       (loop for y from ymin to ymax
             do (incf (aref diagram xmin y))))
      ((= ymin ymax)
       (loop for x from xmin to xmax
             do (incf (aref diagram x ymin)))))))

(defun draw-line-2 (diagram line)
  (let ((xmin (min (line-x1 line) (line-x2 line)))
        (xmax (max (line-x1 line) (line-x2 line)))
        (ymin (min (line-y1 line) (line-y2 line)))
        (ymax (max (line-y1 line) (line-y2 line))))
    (cond
      ((= xmin xmax)
       (loop for y from ymin to ymax
             do (incf (aref diagram xmin y))))
      ((= ymin ymax)
       (loop for x from xmin to xmax
             do (incf (aref diagram x ymin))))
      ((or (and (< (line-x1 line) (line-x2 line))
                (< (line-y1 line) (line-y2 line)))
           (and (< (line-x2 line) (line-x1 line))
                (< (line-y2 line) (line-y1 line))))
       (loop for x from xmin to xmax
             for y from ymin to ymax
             do (incf (aref diagram x y))))
      (t
       (loop for x from xmax downto xmin
             for y from ymin   upto ymax
             do (incf (aref diagram x y)))))))


(defun advent-5-1 ()
  (let ((lines (with-open-file (stream "../input/5/input")
                 (read-lines stream))))
    (loop
      with diagram = (make-array '(1000 1000) :initial-element 0)
      for line in lines
      do (draw-line diagram line)
      finally (return (loop with out = 0
                            for i below 1000
                            do (loop for j below 1000
                                     if (> (aref diagram i j) 1)
                                       do (incf out))
                            finally (return out))))))

(defun advent-5-2 ()
  (let ((lines (with-open-file (stream "../input/5/input")
                 (read-lines stream))))
    (loop
      with diagram = (make-array '(1000 1000) :initial-element 0)
      for line in lines
      do (draw-line-2 diagram line)
      finally (return (loop with out = 0
                            for i below 1000
                            do (loop for j below 1000
                                     if (> (aref diagram i j) 1)
                                       do (incf out))
                            finally (return out))))))
  
