(defun read-commands (stream)
  "Read a stream of commands, where commands ::= (direction magnitude #\newline)*."
  (loop
    for (direction magnitude) = (list (read stream nil) (read stream nil))
    while (and direction magnitude)
    collect (list direction magnitude)))

(defun advent-2-1 ()
  (loop
    with input = (with-open-file (stream "../input/2/input")
                   (read-commands stream))
    with horizontal-position = 0
    with depth = 0
    for (direction magnitude) in input
    do (ecase direction
         (forward (incf horizontal-position magnitude))
         (down (incf depth magnitude))
         (up (decf depth magnitude)))
    finally (return (* horizontal-position depth))))

(defun advent-2-2 ()
  (loop
    with input = (with-open-file (stream "../input/2/input")
                   (read-commands stream))
    with horizontal-position = 0
    with depth = 0
    with aim = 0
    for (direction magnitude) in input
    do (ecase direction
         (forward (incf horizontal-position magnitude) (incf depth (* aim magnitude)))
         (down (incf aim magnitude))
         (up (decf aim magnitude)))
    finally (return (* horizontal-position depth))))
