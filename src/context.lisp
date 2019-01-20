(in-package #:solid-engine)

(defvar *context*)

(defclass context ()
  ((parameters :reader parameters-of)
   (path :initarg :path
	 :reader path-of)))

(defmethod initialize-instance :after ((instance context)
				       &key parameters &allow-other-keys)
  (with-slots ((context-parameters parameters))
      instance
    (setf context-parameters (stack-values parameters))))

(defun stack-values (parameters)
  (let ((hash-table (make-hash-table :test 'equal)))
    (dolist (parameter (reverse parameters) hash-table)
      (destructuring-bind (name . value)
	  parameter
	(push value (gethash name hash-table nil))))))

(defun call-with-context (function path parameters)
  (let ((*context*
	 (make-instance 'context :parameters parameters :path path)))
    (handler-case (funcall function)
      (partial-context (condition) condition))))

(define-condition partial-context ()
  ((parameters :initform (parameters)
	       :reader parameters-of)
   (arguments :initform (arguments)
	      :reader arguments-of)
   (bindings :initform (bindings)
	     :reader bindings-of)
   (context :initarg :context
	    :reader context-of)
   (view :initform (view)
	 :reader view-of)
   (path :initform (path)
	 :reader path-of)))

(define-condition command-is-expected (partial-context)
  ())

(defun pop-path-segment (&optional (context *context*))
  (with-slots (path)
      context
    (when (null path)
      (signal 'command-is-expected :context context))
    (pop path)))

(define-condition value-is-expected (partial-context)
  ((parameter-name :initarg :parameter-name
		   :reader parameter-name-of)))

(defun pop-parameter-value (name &optional (context *context*))
  (let ((hashtable (parameters-of context)))
    (symbol-macrolet ((parameters (gethash name hashtable)))
      (when (null parameters)
	(signal 'value-is-expected
		:parameter-name name
		:context context))
      (pop parameters))))
