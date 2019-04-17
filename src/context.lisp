(in-package #:solid-engine)

(defvar *context*)

(defun context* ()
  *context*)

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

(defun call-with-solid-engine (function path parameters &rest clauses &key &allow-other-keys)
  (let ((*context*
	 (make-instance 'context :parameters parameters :path path)))
    (handler-case (funcall function)
      (reply (condition)
	(let ((view (view-of condition)))
	  (apply (getf clauses (name-of condition))
		 :commands (commands-of condition)
		 :parameters (parameters-of condition)
		 :arguments (arguments-of view)
		 :view (name-of view)
		 (args-of condition)))))))

(define-condition reply ()
  ((name :initarg :name
	 :reader name-of)
   (commands :initarg :commands
	     :reader commands-of)
   (parameters :initarg :parameters
	       :reader parameters-of)
   (view :initarg :view
	 :reader view-of)
   (args :initarg :args
	 :reader args-of)))

(defmacro with-solid-engine ((commands parameters) (&rest clauses) &body body)
  `(call-with-solid-engine #'(lambda () ,@body) ,commands ,parameters
			   ,@(loop for (name args . body) in clauses
				appending `(,name (function (lambda ,args ,@body))))))

(defun list-parameters (command)
  (hash-table-alist
   (parameters-of command)))

(defun reply (name &rest args &key &allow-other-keys)
  (let ((commands (commands)))
    (signal 'reply
	    :name name
	    :commands (mapcar #'name-of commands)
	    :parameters (reduce #'append commands :key #'list-parameters)
	    :view (view)
	    :args args)))

(defun pop-path-segment (&optional (context *context*))
  (with-slots (path)
      context
    (when (null path)
      (reply :command-is-expected))
    (pop path)))

(defun pop-parameter-value (name &optional (context *context*))
  (let ((table (parameters-of context)))
    (symbol-macrolet ((parameters (gethash name table)))
      (when (null parameters)
	(reply :value-is-expected))
      (pop parameters))))
