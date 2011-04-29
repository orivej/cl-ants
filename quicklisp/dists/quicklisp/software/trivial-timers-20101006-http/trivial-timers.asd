(defpackage :trivial-timers.system
  (:use :cl :asdf))

(in-package :trivial-timers.system)

(defsystem :trivial-timers
  :description "Trivial timer library"
  :version "0.1"
  :author "Clinton Ebadi <clinton@unknownlamer.org>"
  :license "public domain"
  :components ((:file "packages")
	       #+sbcl (:file "timers-sbcl")
	       #-sbcl (:file "timers-bt"))
  :serial t
  :depends-on (#-sbcl :bordeaux-threads))