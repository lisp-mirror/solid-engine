;;;; package.lisp

(defpackage #:solid-engine
  (:use #:cl #:alexandria)
  (:export #:answer
	   #:arguments-of
	   #:bindings-of
	   #:call-with-context
	   #:command-of
	   #:context-of
	   #:define-variable
	   #:dispatch-command
	   #:parameter-name-of
	   #:parameter-value
	   #:parameters-of
	   #:path-of
	   #:variants
	   #:view-of
	   #:with-command
	   #:with-variables
	   #:with-view))
