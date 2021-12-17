;;;; Advent of Code 2021 Day 10

;;; We have a file with opening and closing braces.
;;; The braces are (), [], {}, and <>.
;;;
;;; We are looking for "incomplete" and "corrupted" lines.
;;;
;;; The grammar of the file is:
;;; file -> line (newline line)*
;;; line -> expr (whitespace* expr)*
;;; expr -> paren | brace | curly | pointy
;;; paren -> "(" expr ")"
;;; brace -> "[" expr "]"
;;; curly -> "{" expr "}"
;;; pointy -> "<" expr ">"
;;;
;;; We must find the first character that does not match the grammar.
;;;
;;; A simple push state machine will do.
;;; For each line:
;;; - While characters left in line:
;;;   - Read a character
;;;   - If the character is an opener, push it.
;;;   - If the character is a closer:
;;;     - If it matches the top of the stack, pop it.
;;;     - Otherwise, return the value of that character for the line.
;;;   - If we reach the end of the line, return 0.
;;;
;;; Part 2:
;;;
;;; I need to collect the incomplete lines.
;;; Then I need to collect the scores for the lines.
;;; Then I need to sort those scores, and pick the middle one.

(defun read-lines (stream)
  (loop for line = (read-line stream nil)
        while line
        collect line))

(defun advent-10-1 ()
  (let ((lines (with-open-file (stream "../input/10/input")
                 (read-lines stream))))
    (loop
      for line in lines
      sum (loop
            with stack = (list)
            for char across line
            do (case char
                 ((#\( #\[ #\{ #\<) (push char stack))
                 (#\) (if (char= #\( (car stack)) (pop stack) (return 3)))
                 (#\] (if (char= #\[ (car stack)) (pop stack) (return 57)))
                 (#\} (if (char= #\{ (car stack)) (pop stack) (return 1197)))
                 (#\> (if (char= #\< (car stack)) (pop stack) (return 25137))))
            finally (return 0)))))
                
(defun advent-10-2 ()
  (let* ((lines (with-open-file (stream "../input/10/input")
                  (read-lines stream)))
         (incomplete-lines
           (remove-if
            (lambda (line)
              (loop
                with stack = (list)
                for char across line
                do (case char
                     ((#\( #\[ #\{ #\<) (push char stack))
                     (#\) (if (char= #\( (car stack)) (pop stack) (return t)))
                     (#\] (if (char= #\[ (car stack)) (pop stack) (return t)))
                     (#\} (if (char= #\{ (car stack)) (pop stack) (return t)))
                     (#\> (if (char= #\< (car stack)) (pop stack) (return t))))
                finally (return (if stack nil t)))) ; If stack is empty, it's not incomplete.
            lines))
         (scores
           (mapcar
            (lambda (line)
              ;; We now know these are not corrupt.
              (loop
                with stack = (list)
                for char across line
                ;; No (return nil) will fire.
                do (case char
                     ((#\( #\[ #\{ #\<) (push char stack))
                     ((#\) #\] #\} #\>) (pop stack)))
                finally (return (loop
                                  with out = 0
                                  for char in stack
                                  do (setf out (+ (* out 5)
                                                  (ecase char
                                                    (#\( 1)
                                                    (#\[ 2)
                                                    (#\{ 3)
                                                    (#\< 4))))
                                  finally (return out)))))
            incomplete-lines)))
    (pprint incomplete-lines)
    (setf scores (sort scores #'<))
    (nth (floor (/ (length scores) 2)) scores))))
      
