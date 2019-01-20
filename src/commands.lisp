(in-package #:solid-engine)

(defvar *stack* nil)

(defvar *command*)

(defclass command ()
  ((name :initarg :name
	 :reader name-of)
   (parameters :initform (make-hash-table :test 'equal)
	       :reader parameters-of)))

(defmacro dispatch-command (&body commands)
  `(call-dispatch-command #'(lambda (command)
			      (ecase command ,@commands))))

(defun parameters (&optional (commands *stack*))
  (reduce #'append commands :key #'list-parameters :from-end t))

(defun path (&optional (commands *stack*))
  (reduce #'list* commands
	  :initial-value (list)
	  :key #'name-of
	  :from-end t))

(defun command (&optional (command *command*))
  (name-of command))

(defun list-parameters (command)
  (hash-table-plist
   (parameters-of command)))

(define-condition select-command (partial-context)
  ())

(defun call-dispatch-command (function)
  (let* ((command-name
	  (pop-path-segment))
	 (*command*
	  (make-instance 'command :name command-name))
	 (*stack*
	  (list* *command* *stack*)))
    (funcall function command-name))
  (signal 'select-command))

(defun command-arg (name function &optional (command *command*))
  (let ((hash-table (parameters-of command)))
    (symbol-macrolet ((parameter (gethash name hash-table)))
      (multiple-value-bind (value presentp)
	  parameter
	(if (not presentp)
	    (setf parameter (funcall function (pop-parameter-value name)))
	    value)))))
