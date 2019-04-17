(in-package #:solid-engine)

(defvar *view*)

(defclass view ()
  ((name :initarg :name
	 :reader name-of)
   (arguments :initarg :arguments
	      :reader arguments-of)))

(defmacro with-view ((name &rest args &key &allow-other-keys) &body body)
  `(call-with-view #'(lambda () ,@body) (quote ,name) (list ,@args)))

(defun call-with-view (function name arguments)
  (let ((*view*
	 (make-instance 'view :arguments arguments :name name)))
    (funcall function)))

(defun view (&optional (view *view*))
  view)
