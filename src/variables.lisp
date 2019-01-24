(in-package #:solid-engine)

(defvar *variants-fn* #'(lambda () (error "No variants")))

(defmacro define-variable ((view variable) &body body)
  `(call-define-variable #'(lambda () ,@body)
			 (quote ,view)
			 (quote ,variable)))

(defun parameter-value (name &optional (function #'identity))
  (view-param name function))

(defun variants (&optional (variants *variants-fn*))
  (funcall variants))

(defmacro with-variables ((&rest bindings) &body body)
  `(let ,(mapcar #'(lambda (binding)
		     (typecase binding
		       (symbol `(,binding (get-variable-value
					   (quote ,binding))))
		       (list
			(destructuring-bind
			      (name &optional
				    (variants-form nil variants-exist-p))
			    binding
			  (if variants-exist-p
			      `(,name (get-variable-value-with-variants
				       (quote ,name)
				       #'(lambda () ,variants-form)))
			      `(,name (get-variable-value
				       (quote ,name))))))))
		 bindings)
     ,@body))

(defun get-variable-value (variable-name &optional (view (view)))
  (let* ((name (name-of view))
	 (variable-function (get name variable-name))
	 (bindings (bindings-of view)))
    (when (null variable-function)
      (error "Variable ~a for view ~a is not defined" variable-name name))
    (symbol-macrolet ((value (gethash variable-name bindings)))
      (multiple-value-bind (value presentp)
	  value
	(if (not presentp)
	    (setf value (funcall variable-function))
	    value)))))

(defun get-variable-value-with-variants (variable-name function)
  (let ((*variants-fn* function))
    (get-variable-value variable-name)))

(defun call-define-variable (function view variable)
  (setf (get view variable) function))
