(cl:defpackage :trivial-timers
  (:use :common-lisp)
  (:export
   #:timer
   #:timer-name
   #:timer-scheduled-p
   
   #:make-timer
   #:schedule-timer
   #:unschedule-timer

   #:list-all-timers))