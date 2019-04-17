(in-package #:solid-engine)

(defvar *command-stack* nil)

(defvar *command*)

(defclass command ()
  ((name :initarg :name
	 :reader name-of)
   (parameters :initform (make-hash-table :test 'equal)
	       :reader parameters-of)))

(defmacro with-command ((binding) &body body)
  `(call-with-command #'(lambda (,binding)
			  ,@body)))

(defun parameters (&optional (stack *command-stack*))
  (reduce #'append (reverse
		    (rest stack))
	  :key #'(lambda (command)
		   (hash-table-alist
		    (parameters-of command)))))

(defmacro dispatch-command (&body commands)
  `(with-command (command)
     (ecase command ,@commands)))

(defun call-with-command (function)
  (let* ((command-name
	  (pop-path-segment))
	 (*command*
	  (make-instance 'command :name command-name))
	 (*command-stack*
	  (list* *command* *command-stack*)))
    (funcall function command-name))
  (reply :end-of-stack))

(defun commands (&optional (commands *command-stack*))
  (reverse commands))

(defun command (&optional (command *command*))
  command)

(defun parameter-value (name &optional (function #'identity))
  (let ((hash-table (parameters-of (command))))
    (symbol-macrolet ((parameter (gethash name hash-table)))
      (multiple-value-bind (value presentp)
	  parameter
	(if (not presentp)
	    (setf parameter
		  (funcall function
			   (pop-parameter-value name)))
	    value)))))
