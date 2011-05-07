;;;; ants-bot.lisp

(in-package #:ants-bot)

(defparameter *parameters* nil)

(defun read-parameters ()
  "Read parameters into an alist"
  (iter (for op in-stream *standard-input*)
        (until (eq op 'ready))
        (collect (cons op (read)))))

(defun parse-parameters ()
  (setf *parameters* (read-parameters))
  (format t "go~%"))

(defun read-pair ()
  "Read 1 2 into (1 . 2)"
  (cons (read) (read)))

(defun read-triple ()
  "Read 1 2 3 into (3 . (1 . 2))"
  (let ((pair (read-pair)))
    (cons (read) pair)))

(defun read-turn ()
  "Read f 1 1 w 3 3 a 4 4 0 d 5 6 1 go into ((FOOD . ((1 . 1))) (WATER . ((3 . 3))) (ANT . ((0 4 . 4))) (DEAD . ((1 5 . 6))))"
  (iter (for op in-stream *standard-input*)
        (until (eq op 'go))
        (case op
          (f (collect (read-pair) into food))
          (w (collect (read-pair) into water))
          (a (collect (read-triple) into ant))
          (d (collect (read-triple) into dead))
          ;; unknown input must be skipped
          (otherwise (read-line)))
        (finally (return `((food . ,food)
                           (water . ,water)
                           (ant . ,ant)
                           (dead . ,dead))))))

(defun process-turn (turnNo)
  (declare (ignore turnNo))
  (read-turn)
  (format t "go~%"))

(defun ants-bot ()
  "Entry point.  Begin and win."
  (parse-parameters)
  (iter (for op in-stream *standard-input*)
        (until (eq op 'end))
        (process-turn (read))))
