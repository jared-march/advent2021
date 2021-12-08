(defun ignore-comma (stream char)
  "Do nothing if a comma is read, as if it was whitespace."
  (declare (ignore stream char))
  (values))

(defun read-bingo-input (stream)
  "Reads a bingo stream. 

Outputs list of called numbers, and a list of 2D arrays of boards."
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, #'ignore-comma)
    (let* ((called-numbers
             (with-input-from-string (called-numbers-line (read-line stream))
               (loop for num = (read called-numbers-line nil)
                     while num
                     collect num)))
           (boards
             (loop
               for board = (loop
                             with board = (make-array '(5 5))
                             for i from 0 below 25
                             for cell = (read stream nil)
                             do (setf (aref board (floor (/ i 5)) (mod i 5))
                                      cell)
                             unless cell
                               do (return)
                             finally (return board))
               while board
               collect board)))
      
      (list called-numbers boards))))

(defun mark-number (board number)
  "Search a board for a number. If it finds it, mark it nil."
  (loop for i below 5
        do (loop for j below 5
                 if (and (aref board i j)
                         (= (aref board i j) number))
                   do (setf (aref board i j) nil))))

(defun board-win-p (board)
  "If a board is a winning board, return the board. Otherwise, return nil."
  (if (or (loop for i below 5
                  thereis (loop for j below 5
                                never (aref board i j)))
          (loop for i below 5
                  thereis (loop for j below 5
                                never (aref board j i))))
      board))

(defun board-score (board)
  (loop
    with out = 0
    for i below 5
    do (loop
         for j below 5
         if (aref board i j)
           do (incf out (aref board i j)))
    finally (return out)))

(defun advent-4-1 ()
  (let* ((input (with-open-file (stream "../input/4/input")
                  (read-bingo-input stream)))
         (called-numbers (first input))
         (boards (second input))
         (winning-call nil)
         (winning-board (loop
                          for call in called-numbers
                          do (setf winning-call call)
                          do (loop for board in boards
                                   do (mark-number board call))
                            thereis (loop for board in boards
                                            thereis (board-win-p board))))
         (winning-board-score (board-score winning-board)))
    (* winning-call winning-board-score)))
                                         
(defun advent-4-2 ()
  (let* ((input (with-open-file (stream "../input/4/input")
                  (read-bingo-input stream)))
         (called-numbers (first input))
         (boards (second input))
         (winning-call nil)
         (losing-board (loop
                         for call in called-numbers
                         while (or (cdr boards) ; while more than one board
                                   (not (board-win-p (car boards))))
                         do (setf winning-call call)
                         do (loop for board in boards
                                  do (mark-number board call))
                         if (cdr boards)
                           do (setf boards (delete-if #'board-win-p boards))
                         finally (return (car boards))))
                            
         (losing-board-score (board-score losing-board)))
    (* winning-call losing-board-score)))
      
