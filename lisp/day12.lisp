;;;; Advent of Code Day 12

;;; We have a graph of nodes.
;;; One node is 'start', one node is 'end'.
;;; Other nodes are either big, with capitalized names,
;;; or small, with lower case names.
;;; First, read the input and create the graph.
;;; Find each path from start to end traversing small nodes at most once.

;;; The paths are sequences of nodes.
;;; The graph can be represented as a map from a node to everything it's connected to.
;;; We can make a recursive solution f(path).
;;; If f(end...), return 1.
;;; If f(small-node...), and small-node ∈ path, return 0.
;;; Else, return the sum of f(connected-node...), for all connected nodes.

;;; Part 2
;;;
;;; One small cave may be visited twice, but not start or end!
;;; An additional parameter is probably the least effort for this.

(defun ignore-hyphen (stream char)
  (declare (ignore stream char))
  (values))

(defun read-graph (stream)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\- #'ignore-hyphen)
    (setf (readtable-case *readtable*) :preserve) ; case sensitive, please
    (loop
      with graph = (list) ; An alist from node to connected nodes.
      for left = (read stream nil)
      for right = (read stream nil)
      while right
      if (not (assoc left graph))
        do (setf graph (acons left nil graph))
      if (not (assoc right graph))
        do (setf graph (acons right nil graph))
      do (let ((left-node (assoc left graph)))
           (rplacd left-node (cons right (cdr left-node))))
      do (let ((right-node (assoc right graph)))
           (rplacd right-node (cons left (cdr right-node))))
      finally (return (loop
                        for kv in graph
                        do (rplacd kv (delete-duplicates (cdr kv)))
                        finally (return graph))))))

(defun graph-paths (graph path)
  (cond
    ((eq (car path) '|end|) 1) ; valid path end
    ((and (lower-case-p (aref (symbol-name (car path)) 1)) ; small cave?
          (member (car path) (cdr path))) ; been there before?
     0) ; invalid path
     (t (loop for next in (cdr (assoc (car path) graph))
              sum (graph-paths graph (cons next path))))))

(defun advent-12-1 ()
  (let ((graph (with-open-file (stream "../input/12/input")
                 (read-graph stream))))
    (graph-paths graph '(|start|))))

(defun graph-paths-2 (graph path special-cave)
  (cond
    ;; valid path end case
    ((eq (car path) '|end|) 1)
    ;; first duplicate small cave case
    ((and (lower-case-p (aref (symbol-name (car path)) 1)) ; small cave?
          (member (car path) (cdr path)) ; been there before?
          (not (eq (car path) '|start|)) ; no start allowed
          (null special-cave)) ; no special yet
     (loop for next in (cdr (assoc (car path) graph))
           sum (graph-paths-2 graph (cons next path) (car path))))
    ;; regular duplicate small cave case
    ((and (lower-case-p (aref (symbol-name (car path)) 1)) ; small cave?
          (member (car path) (cdr path))) ; been there before?
     0) ; invalid path
    ;; regular connecting path case
    (t (loop for next in (cdr (assoc (car path) graph))
             sum (graph-paths-2 graph (cons next path) special-cave)))))
         
          
     
    

(defun advent-12-2 ()
  (let ((graph (with-open-file (stream "../input/12/input")
                 (read-graph stream))))
    (graph-paths-2 graph '(|start|) nil)))
