;;;; Advent of Code 2021 Day 17

;;; Part 1
;;;
;;; This has a completely mathematical solution.
;;; You shoot at 0, 0, and it must be the shot with the highest possible y velocity.
;;; If you think about it, your shot will hit position (x, 0) with velocity -y.
;;; So, you want the y value that will travel from (x, 0) to (?, -(y + 1)),
;;; where -(y + 1) is also the lowest point of the target.
;;; You can find this with:
;;; Given y = abs(lowest y coordinate of target):
;;; yv = y * (y - 1) / 2
;;;
;;; I did this by hand. No code!

;;; Day 2
;;;
;;; Alright, now for real code. We have a range of valid initial x and y velocities.
;;; ymax = abs(lowest point of target) - 1
;;; ymin = lowest point of target
;;; xmax = rightmost point of target
;;; xmin = 0, whatever
;;;
;;; Stop tracing if y < ymin, x > xmax, or x < xmin.

(defun shot-hit-p (initial-x-velocity initial-y-velocity
                   target-xmin target-xmax
                   target-ymin target-ymax)
  (loop with x = 0
        with x-velocity = initial-x-velocity
        with y = 0
        with y-velocity = initial-y-velocity
        while (>= y target-ymin)
        if (and (<= target-xmin x target-xmax)
                (<= target-ymin y target-ymax))
          do (return t)
        do (progn (setf x (+ x x-velocity))
                  (setf y (+ y y-velocity))
                  (setf x-velocity (max 0 (1- x-velocity)))
                  (setf y-velocity (1- y-velocity)))
        finally (return nil)))


(defun advent-19-2 ()
  ;; hardcore cheating
  (let* ((target-xmin 192)
         (target-xmax 251)
         (target-ymin -89)
         (target-ymax -59)
         (ymin target-ymin)
         (ymax (1- (abs target-ymin)))
         (xmax target-xmax)
         (xmin 0))
    (loop for y from ymin upto ymax
          sum (loop for x from xmin upto xmax
                    sum (if (shot-hit-p x y
                                        target-xmin target-xmax
                                        target-ymin target-ymax)
                            1
                            0)))))
