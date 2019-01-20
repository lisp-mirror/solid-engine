(in-package #:solid-engine)

(defvar *view*)

(defclass view ()
  ((name :initarg :name
	 :reader name-of)
   (bindings :initform (make-bindings-table)
	     :reader bindings-of)
   (arguments :initarg :arguments
	      :reader arguments-of)))

(defmacro with-view ((name &rest args &key &allow-other-keys) &body body)
  `(call-with-view #'(lambda () ,@body) (quote ,name) ,@args))

(defun call-with-view (function name &rest arguments)
  (let ((*view* (make-instance 'view :arguments arguments :name name)))
    (funcall function)))

(defun make-bindings-table ()
  (let ((hash-table (make-hash-table)))
    (when (boundp '*view*)
      (loop for key being the hash-keys of (bindings-of *view*)
	 using (hash-value value)
	 do (setf (gethash key hash-table) value)))
    hash-table))

(defun bindings (&optional (view *view*))
  (hash-table-plist
   (bindings-of view)))

(defun arguments (&optional (view *view*))
  (arguments-of view))

(defun view (&optional (view *view*))
  (name-of view))
