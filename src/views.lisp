(in-package #:solid-engine)

(defvar *view-stack* nil)

(defclass view ()
  ((name :initarg :name
	 :reader name-of)
   (parameters :initform (make-hash-table :test 'equal)
	       :reader parameters-of)
   (arguments :initarg :arguments
	      :reader arguments-of)
   (bindings :initform (make-bindings-table)
	     :reader bindings-of)))

(defmacro with-view ((name &rest args &key &allow-other-keys) &body body)
  `(call-with-view #'(lambda () ,@body) (quote ,name) (list ,@args)))

(defun call-with-view (function name arguments &optional (stack *view-stack*))
  (let ((*view-stack*
	 (list*
	  (make-instance 'view :arguments arguments :name name)	stack)))
    (funcall function)))

(defun parameters (&optional (stack *view-stack*))
  (reduce #'append (rest stack)
	  :key #'(lambda (view)
		   (hash-table-plist
		    (parameters-of view)))
	  :from-end t))

(defun view (&optional (stack *view-stack*))
  (first stack))

(defun bindings (&optional (view (view)))
  (when (not (null view))
    (hash-table-plist
     (bindings-of view))))

(defun arguments (&optional (view (view)))
  (when (not (null view))
    (arguments-of view)))

(defun make-bindings-table (&optional (view (view)))
  (let ((table (make-hash-table)))
    (when (not (null view))
      (loop for key being the hash-keys of (bindings-of view)
	 using (hash-value value)
	 do (setf (gethash key table) value)))
    table))

(defun view-param (name function &optional (view (view)))
  (let ((hash-table (parameters-of view)))
    (symbol-macrolet ((parameter (gethash name hash-table)))
      (multiple-value-bind (value presentp)
	  parameter
	(if (not presentp)
	    (setf parameter
		  (funcall function
			   (pop-parameter-value name)))
	    value)))))
