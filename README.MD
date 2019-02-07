# Solid-engine - The Common Lisp MVC stack-based controller

## Problems and solution concept 

HTTP-protocol is stateless. If we want to do complex actions, for
example: wizard dialog on the series of pages or, more importantly,
manage presentation from server, we must save intermediate state. For
storing state can be used some techics: storing session in database or
server memory, storing some data on page by using async requests or by
using cookies. All of that techics have their drowback, storing data
on client fraught with security breach, storing session in server
memory imposes restrictions on the amount of memory and in addition
there is a need to keep track of the lifetime of a session. Moreover,
the user will not be able to visit the page after the expiration of
the session. There is a need to restore the state by information
available in the URL.

In addition to this problems, an effective mechanism is needed to
control the presentation from the server (controller).

Solid-engine is developed to solve this problems. The idea is bored
from SICP (amb-operator). As opposed to amb-operator, where choice
controlled by amb-operator, in Solid-engine choice provided by
user. In point of program view, selection also non-deterministic. It
looks like when implementing a controller code, developer must place
choice points.

If we imagine user's work on site as process, then we can code user's
workflow as program code with points of choice in some places. In that
places we have information about objects that user must select (from
set of objects) or provide (simple type values) to continue
execution. Each choice point is place where user must provide values
for variables. Variable values for choice points is stack up. This
stack client must send to server to execute. The response from server
contains other stack, that client must sent with some additional
user's choiced values. Received client's values stack on server using
to provide variable values in choice points. Process of executing
program start on each request and lasts until the stack is
empty. Thereafter variables binding form send signal and server
responds to client.

Stack on client can be stored in URL and form fields. To avoid
repeating of server side effect actions stack returned to client did
not contain information for visit lefted code branch. Becouse calling
context playback is explicit, not indirect additional checking no more
needed.

## Implementation

Solid-engine is developed for use with HTTP-protocol, but fits for use
with any stateless-protocol. It view- and server-agnostic. You can
freely choose web-server and presentation layer by implementing thin
integration code.

Solid-engine is controller in MVC pattern and can manage model and
view. Choice point represent "view" in MVC pattern.

To implement choice points and binding information to make response to
client written four macro. Choice stack can be stored in URL (as query
parameters and parts of path) and POST-parameters.

## Usage

### Integration

First of all, you must integrate it with your web-server and
presentation-layer.

For Hunchentoot and cl-closure-tmplates integration code listed in the
end of file.

### Using

Solid-engine provide some macros and functions:

#### Macro

(with-view (name &key parameters) &body) - declares current view and
parameters to transfer to it.

(with-command (name) &body) - bind's command parameter (see example
above).

(dispatch-command &body) - 'ecase' form for dispatching command:
(with-command (command) (ecase command ...)).

(with-variables (bindings) &body) - binds stack parameters. Bindings
is list of symbol or two element form (name variants). Variants is
evaluable syntax expression that return list and using is
"define-variable".

(define-variable (view name) &body) - defines variable in context of
view. All of variable using by (with-variables...) must be defined.

### Functions

(variants) - list of objects, provided by variants in (with-variables
form).

(call-with-context function commands-stack parameters-stack) -> answer
- called to begin exection. Function - is arguments-less function
(entry-point). Commands-stack - list of commands. Parameters-stack
alist of parameters (as Hunchentoot provides by (get-parameters*) or
(post-parameters*).

(view-of answer) - name of view
(arguments-of answer) - arguments that passed by `with-view' macro
(parameters-of answer) - stack (list) of used parameters from stack
(path-of answer) - stack (list) of used commands

One of parameters contain information about action, it named is
"command".

## Example:

(defpackage #:web-app
  (:use #:alexandria
  	#:cl
	#:closure-template
	#:cl-ppcre
	#:hunchentoot
	#:solid-engine))

(in-package #:web-app)

(defclass application (acceptor)
  ())

(defun start-web-app (&rest args &key &allow-other-keys)
  (start
   (apply #'make-instance 'application args)))

(defmethod acceptor-dispatch-request ((acceptor application) request)
  (let ((script-name (script-name* request)))
    (cond
      ((or
	(starts-with-subseq "/img" script-name)
	(string= "/favicon.ico" script-name))
       (call-next-method))
      (t
       (let ((path (make-path script-name))
	     (parameters (list-parameters request)))
	 (reply
	  (call-with-context #'main path parameters)))))))

(defun reply (answer)
  (let* ((parameters
	  (parameters-of answer))
	 (script-name
	  (format nil "/~{~(~a~)~^/~}" (path-of answer)))
	 (query-string
	  (format nil "~{~(~a~)=~a~^&~}" (alist-plist parameters)))
	 (*injected-data*
	  (list :parameters
		(mapcar #'(lambda (parameter)
			    (list :name (first parameter)
				  :value (rest parameter)))
			parameters)
		:query-string query-string
		:script-name script-name)))
    (case (request-method*)
      (:get
       (funcall
	(symbol-function
	 (find-symbol
	  (string (view-of answer))
	  (find-package "WEB-APP")))
	(arguments-of answer)))
      (:post
       (redirect (format nil "~a~@[?~a~]" script-name query-string)
		 :code +http-see-other+)))))

(defun make-path (script-name)
  (let ((package (find-package "WEB-APP")))
    (mapcar #'(lambda (name)
		(let ((symbol-name (string-upcase name)))
		  (or
		   (find-symbol symbol-name package)
		   (error "Symbol path with name '~a' not found in package ~a"
			  name package))))
	    (split "/+" (subseq script-name 1)))))

(defun list-parameters (request)
  (case (request-method* request)
    (:get (get-parameters* request))
    (:post (post-parameters* request))))
