(in-package #:solid-engine)

(defvar *variants*)

(defmacro define-variable ((view variable) &body body)
  `(call-define-variable #'(lambda () ,@body)
			 (quote ,view)
			 (quote ,variable)))

(defun variants (&optional (variants *variants*))
  variants)

(defmacro with-variables ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (binding)
		     (typecase binding
		       (symbol
			`(,binding (bind-variable (quote ,binding))))
		       (list
			(destructuring-bind (name &optional variants)
			    binding
			  `(,name (bind-variable (quote ,name) ,variants))))))
		 bindings)
     ,@body))

(defun bind-variable (name &optional variants)
  (let ((*variants* variants))
    (get-variable-value name)))

(defun get-variable-value (variable-name &optional (view (view)))
  (let* ((name (name-of view))
	 (variable-function (get name variable-name)))
    (when (null variable-function)
      (error "Variable ~a for command ~a is not defined" variable-name name))
    (funcall variable-function)))

(defun call-define-variable (function view variable)
  (setf (get view variable) function))
