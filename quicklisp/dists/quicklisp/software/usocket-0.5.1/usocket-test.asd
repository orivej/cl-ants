;;;; -*- Mode: Lisp -*-
;;;; $Id: usocket-test.asd 46 2006-02-06 20:50:07Z ehuelsmann $
;;;; $URL: svn+ssh://common-lisp.net/project/usocket/svn/usocket/trunk/test/usocket-test.asd $

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(unless (find-package ':usocket-system)
  (make-package ':usocket-system
		:use '(:cl :asdf)))

(in-package :usocket-system)

(defsystem usocket-test
    :name "usocket test"
    :author "Erik Enge"
    :maintainer "Chun Tian (binghe)"
    :version "0.2.0"
    :licence "MIT"
    :description "Tests for usocket"
    :depends-on (:usocket
                 :rt)
    :components ((:module "test"
		  :serial t
		  :components ((:file "package")
			       (:file "test-usocket")
			       (:file "test-condition")
			       (:file "test-datagram")))))

(defmethod perform ((op test-op) (c (eql (find-system :usocket-test))))
  (funcall (intern "DO-TESTS" "USOCKET-TEST")))
