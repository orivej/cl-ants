;;; Global (local + bundled for upload) usage templates

#-quicklisp
(load "quicklisp/setup.lisp")
(asdf:initialize-source-registry
 '(:source-registry
   :ignore-inherited-configuration
   (:directory :here)))

;;; Local usage templates

;(ql:update-client)
;(ql:update-all-dists)
