(in-package #:solid-engine)

(defvar *command-stack* nil)

(define-condition select-command (partial-context)
  ())

(defmacro with-command ((binding) &body body)
  `(call-with-command #'(lambda (,binding)
			  ,@body)))

(defmacro dispatch-command (&body commands)
  `(call-with-command #'(lambda (command)
			  (ecase command ,@commands))))

(defun call-with-command (function)
  (let* ((command-name (pop-path-segment))
	 (*command-stack* (list* command-name *command-stack*)))
    (funcall function command-name))
  (signal 'select-command))

(defun path (&optional (commands *command-stack*))
  (reverse commands))
