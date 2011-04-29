(in-package :trivial-timers)

(shadowing-import '(sb-ext:timer
		    sb-ext:timer-name
		    sb-ext:timer-scheduled-p
		    
		    sb-ext:make-timer
		    sb-ext:schedule-timer
		    sb-ext:unschedule-timer

		    sb-ext:list-all-timers))

(export '(timer
	  timer-name
	  timer-scheduled-p
	  
	  make-timer
	  schedule-timer
	  unschedule-timer
	  
	  list-all-timers))