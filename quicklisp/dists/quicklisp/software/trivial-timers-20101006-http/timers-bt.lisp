;;;; a timer facility based heavily on the timer package by Zach Beane
;;;; ported from SBCL to merely rely upon bordeaux-threads by Clinton Ebadi

;;;; This software was part of the SBCL system. 
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package :trivial-timers)

;;; Heap (for the priority queue)

(declaim (inline heap-parent heap-left heap-right))

(defun heap-parent (i)
  (ash (1- i) -1))

(defun heap-left (i)
  (1+ (ash i 1)))

(defun heap-right (i)
  (+ 2 (ash i 1)))

(defun heapify (heap start &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (let ((l (heap-left start))
          (r (heap-right start))
          (size (length heap))
          largest)
      (setf largest (if (and (< l size)
                             (not (ge (key (aref heap start))
                                      (key (aref heap l)))))
                        l
                        start))
      (when (and (< r size)
                 (not (ge (key (aref heap largest))
                          (key (aref heap r)))))
        (setf largest r))
      (when (/= largest start)
        (rotatef (aref heap largest) (aref heap start))
        (heapify heap largest :key key :test test)))
    heap))

(defun heap-insert (heap new-item &key (key #'identity) (test #'>=))
  (declare (function key test))
  (flet ((key (obj) (funcall key obj))
         (ge (i j) (funcall test i j)))
    (vector-push-extend nil heap)
    (loop for i = (1- (length heap)) then parent-i
          for parent-i = (heap-parent i)
          while (and (> i 0)
                     (not (ge (key (aref heap parent-i))
                              (key new-item))))
          do (setf (aref heap i) (aref heap parent-i))
          finally (setf (aref heap i) new-item)
          (return-from heap-insert i))))

(defun heap-maximum (heap)
  (unless (zerop (length heap))
    (aref heap 0)))

(defun heap-extract (heap i &key (key #'identity) (test #'>=))
  (unless (> (length heap) i)
    (error "Heap underflow"))
  (prog1
      (aref heap i)
    (setf (aref heap i) (aref heap (1- (length heap))))
    (decf (fill-pointer heap))
    (heapify heap i :key key :test test)))

(defun heap-extract-maximum (heap &key (key #'identity) (test #'>=))
  (heap-extract heap 0 :key key :test test))

;;; Priority queue

(defstruct (priority-queue
             (:conc-name %pqueue-)
             (:constructor %make-priority-queue))
  contents
  keyfun)

(defun make-priority-queue (&key (key #'identity) (element-type t))
  (let ((contents (make-array 100
                              :adjustable t
                              :fill-pointer 0
                              :element-type element-type)))
    (%make-priority-queue :keyfun key
                          :contents contents)))

(defmethod print-object ((object priority-queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~[empty~:;~:*~D item~:P~]"
            (length (%pqueue-contents object)))))

(defun priority-queue-maximum (priority-queue)
  "Return the item in PRIORITY-QUEUE with the largest key."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue)))
    (unless (zerop (length contents))
      (heap-maximum contents))))

(defun priority-queue-extract-maximum (priority-queue)
  "Remove and return the item in PRIORITY-QUEUE with the largest key."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue))
                    (keyfun (%pqueue-keyfun priority-queue)))
    (unless (zerop (length contents))
      (heap-extract-maximum contents :key keyfun :test #'<=))))

(defun priority-queue-insert (priority-queue new-item)
  "Add NEW-ITEM to PRIOIRITY-QUEUE."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue))
                    (keyfun (%pqueue-keyfun priority-queue)))
    (heap-insert contents new-item :key keyfun :test #'<=)))

(defun priority-queue-empty-p (priority-queue)
  (zerop (length (%pqueue-contents priority-queue))))

(defun priority-queue-remove (priority-queue item &key (test #'eq))
  "Remove and return ITEM from PRIORITY-QUEUE."
  (symbol-macrolet ((contents (%pqueue-contents priority-queue))
                    (keyfun (%pqueue-keyfun priority-queue)))
    (let ((i (position item contents :test test)))
      (when i
        (heap-extract contents i :key keyfun :test #'<=)
        i))))

;;; thread utility

(defun make-cancellable-interruptor (function)
  ;; return a list of two functions: one that does the same as
  ;; FUNCTION until the other is called, from when it does nothing.
  (let ((mutex (bt:make-lock))
        (cancelled-p nil))
    (list
     #'(lambda ()
         (bt:with-recursive-lock-held (mutex)
           (unless cancelled-p
             (funcall function))))
     #'(lambda ()
         (bt:with-recursive-lock-held (mutex)
           (setq cancelled-p t))))))

;;; timers

(defstruct (timer
             (:conc-name %timer-)
             (:constructor %make-timer))
  "Timer type. Do not rely on timers being structs as it may change in
future versions."
  name
  function
  expire-time
  repeat-interval
  (thread nil #| :type (or bt:thread (member t nil)) |#)
  interrupt-function
  cancel-function)

(defmethod print-object ((timer timer) stream)
  (let ((name (%timer-name timer)))
    (if name
        (print-unreadable-object (timer stream :type t :identity t)
          (prin1 name stream))
        (print-unreadable-object (timer stream :type t :identity t)
          ;; body is empty => there is only one space between type and
          ;; identity
          ))))

(defun make-timer (function &key name (thread (bt:current-thread)))
  "Create a timer object that's when scheduled runs FUNCTION. If
THREAD is a thread then that thread is to be interrupted with
FUNCTION. If THREAD is T then a new thread is created each timer
FUNCTION is run. If THREAD is NIL then FUNCTION can be run in any
thread."
  (%make-timer :name name :function function :thread thread))

(defun timer-name (timer)
  "Return the name of TIMER."
  (%timer-name timer))

(defun timer-scheduled-p (timer &key (delta 0))
  "See if TIMER will still need to be triggered after DELTA seconds
from now. For timers with a repeat interval it returns true."
  (symbol-macrolet ((expire-time (%timer-expire-time timer))
                    (repeat-interval (%timer-repeat-interval timer)))
      (or (and repeat-interval (plusp repeat-interval))
          (and expire-time
               (<= (+ (get-internal-real-time) delta)
                   expire-time)))))

;;; The scheduler

(defvar *scheduler-lock* (bt:make-lock "Scheduler lock"))
(defvar *scheduler-cond* (bt:make-condition-variable))
(defvar *scheduler-thread* nil)

(defmacro with-scheduler-lock ((&optional) &body body)
  ;; Don't let the SIGALRM handler mess things up.
  `(bt:with-lock-held (*scheduler-lock*)
     ,@body))

(defun set-system-timer ()
  (bt:condition-notify *scheduler-cond*))

(defparameter *schedule* (make-priority-queue :key #'%timer-expire-time))

(defun peek-schedule ()
  (priority-queue-maximum *schedule*))

(defun time-left (timer)
  (- (%timer-expire-time timer) (get-universal-time)))

;;; real time conversion

(defun delta->real (delta)
  (floor (* delta internal-time-units-per-second)))

;;; Public interface

(defun %schedule-timer (timer)
  (let ((changed-p nil)
        (old-position (priority-queue-remove *schedule* timer)))
    ;; Make sure interruptors are cancelled even if this timer was
    ;; scheduled again since our last attempt.
    (when old-position
      (funcall (%timer-cancel-function timer)))
    (when (eql 0 old-position)
      (setq changed-p t))
    (when (zerop (priority-queue-insert *schedule* timer))
      (setq changed-p t))
    (setf (values (%timer-interrupt-function timer)
                  (%timer-cancel-function timer))
          (values-list (make-cancellable-interruptor
                        (%timer-function timer))))
    (when changed-p
      (set-system-timer)))
  (values))

(defun schedule-timer (timer time &key repeat-interval absolute-p)
  "Schedule TIMER to be triggered at TIME. If ABSOLUTE-P then TIME is
universal time, but non-integral values are also allowed, else TIME is
measured as the number of seconds from the current time. If
REPEAT-INTERVAL is given, TIMER is automatically rescheduled upon
expiry."
  (unless *scheduler-thread*
    (start-timers))
  ;; CANCEL-FUNCTION may block until all interruptors finish, let's
  ;; try to cancel without the scheduler lock first.
  (when (%timer-cancel-function timer)
    (funcall (%timer-cancel-function timer)))
  (with-scheduler-lock ()
    (setf (%timer-expire-time timer) (if absolute-p
					 time
					 (+ time (get-universal-time)))
          (%timer-repeat-interval timer) repeat-interval)
    (%schedule-timer timer)))

(defun unschedule-timer (timer)
  "Cancel TIMER. Once this function returns it is guaranteed that
TIMER shall not be triggered again and there are no unfinished
triggers."
  (let ((cancel-function (%timer-cancel-function timer)))
    (when cancel-function
      (funcall cancel-function)))
  (with-scheduler-lock ()
    (setf (%timer-expire-time timer) nil
          (%timer-repeat-interval timer) nil)
    (let ((old-position (priority-queue-remove *schedule* timer)))
      (when old-position
        (funcall (%timer-cancel-function timer)))
      (when (eql 0 old-position)
        (set-system-timer))))
  (values))

(defun list-all-timers ()
  "Return a list of all timers in the system."
  (with-scheduler-lock ()
    (concatenate 'list (%pqueue-contents *schedule*))))

;;; Not public, but related

(defun reschedule-timer (timer)
  (let ((thread (%timer-thread timer)))
    (if (and (bt:threadp thread) (not (bt:thread-alive-p thread)))
        (unschedule-timer timer)
        (with-scheduler-lock ()
          (setf (%timer-expire-time timer) (+ (get-universal-time)
                                              (%timer-repeat-interval timer)))
          (%schedule-timer timer)))))

;;; Expiring timers

(defun run-timer (timer)
  (symbol-macrolet ((function (%timer-function timer))
                    (repeat-interval (%timer-repeat-interval timer))
                    (thread (%timer-thread timer)))
    (when repeat-interval
      (reschedule-timer timer))
    (cond ((null thread)
           (funcall function))
          ((eq t thread)
           (bt:make-thread function))
          (t (bt:interrupt-thread thread function)))))

;; Main scheduler loop
(defun run-expired-timers ()
  (let (timer)
    (loop
       (with-scheduler-lock ()
	 (setq timer (peek-schedule))
	 (cond ((and timer
		     (< (get-universal-time)
			(%timer-expire-time timer)))
		(handler-case 
		    (bt:with-timeout ((- (%timer-expire-time timer)
					 (get-universal-time)))
		      (bt:condition-wait *scheduler-cond* *scheduler-lock*))
		  (bt:timeout () nil)))
	       (timer
		(assert (eq timer (priority-queue-extract-maximum *schedule*)))
		(bt:release-lock *scheduler-lock*)
		;; run the timer without the lock
		(run-timer timer)
		(bt:acquire-lock *scheduler-lock*))
	       ((not timer)
		(bt:condition-wait *scheduler-cond* *scheduler-lock*)))))))

(defun start-timers ()
  (setf *scheduler-thread*
	(bt:make-thread #'run-expired-timers :name "Timer Scheduler")))

(defun stop-timers ()
  (when *scheduler-thread*
    (bt:destroy-thread *scheduler-thread*)))
