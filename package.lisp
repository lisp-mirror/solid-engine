;;;; package.lisp

(defpackage #:solid-engine
  (:use #:cl #:alexandria)
  (:export #:bind-variable
	   #:define-variable
	   #:dispatch-command
	   #:parameter-value
	   #:reply
	   #:variants
	   #:with-command
	   #:with-solid-engine
	   #:with-variables
	   #:with-view))
