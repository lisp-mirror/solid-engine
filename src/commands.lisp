(in-package #:solid-engine)

(defvar *command-stack* nil)

(define-condition select-command (partial-context)
  ())

(defmacro dispatch-command (&body commands)
  `(call-dispatch-command #'(lambda (command)
			      (ecase command ,@commands))))

(defun call-dispatch-command (function)
  (let* ((command-name (pop-path-segment))
	 (*command-stack* (list* command-name *command-stack*)))
    (funcall function command-name))
  (signal 'select-command))

(defun path (&optional (commands *command-stack*))
  (reverse commands))
