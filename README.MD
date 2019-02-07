# Solid-engine - The Common Lisp stack-based application controller

## Problems and solution concept 
HTTP-protocol is stateless. If we want to do complex actions, for example: wizard dialog on the series of pages or, more importantly, manage presentation from server, we must save intermediate state. For storing state can be used some techics: storing session in database or server memory, storing some data on page by using async requests or by using cookies. All of that techics have their drowback, storing data on client fraught with security breach, storing session in server memory imposes restrictions on the amount of memory and in addition there is a need to keep track of the lifetime of a session. Moreover, the user will not be able to visit the page after the expiration of the session. There is a need to restore the state by information available in the URL.
In addition to this problems, an effective mechanism is needed to control the presentation from the server (appplication controller).

Solid-engine is developed to solve this problems. What is a application controller you can read from Martin Fauler "Patterns of Enterprise Application Architecture". The idea of implementation is bored from SICP (amb-operator). As opposed to amb-operator, where choice controlled by amb-operator, in Solid-engine choice provided by user. In point of program view, selection also non-deterministic --- developer just place choice points in controller code.
If we imagine user's work in web-apllication as process, then we can code user's workflow as program code with points of choice in some places. In that places we have information about objects that user must select (from set of objects) or provide (simple type values) to continue execution. Each choice point is place where user must provide values for variables. Variable values for choice points is stack up and client must send it to server for execution. The response from server contains other stack, that client must sent with some additional user's choiced values. Received client's stack on provide
variable values for choice points. Process of executing program start on each request and lasts until the stack is empty. Thereafter variables binding form send return information abount stack and presentation.
Stack on client can be stored in URL and form fields. To avoid repeating of server side effect actions retuerned to client stack did not contain information for visit lefted (side effect) code branch.
Additional advantages is preserving context of logic execution, any additional checking not needed. Call stack playback eliminates possibility of wrong logic execution.

## Implementation
Solid-engine is developed for use with HTTP-protocol, but fits for use with any stateless-protocol. It presentation- and server-agnostic. You can freely choose (web-)server and presentation layer by implementing thin integration code.
For http-protocol choice stack can be stored in URL (as query parameters and parts of path) and POST-parameters.

## Usage

### Integration
First of all, you must integrate it with your (web-)server and presentation-layer.
For Hunchentoot and cl-closure-templates integration code listed above:

```common-lisp
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
```

### Using
Solid-engine provide some macros and functions:



*Function* __CALL-WITH-CONTEXT__

__Syntax:__

__call-with-context__ *function commands-stack parameters-stack => result*

__Arguments and Values__

*function* --- arguments-less function, entry-point of application.

*commands-stack* --- list of commands. Command is stack variable, but useful to implement them separate from other parameters.

*parameters-stack* --- alist of other parameters (as Hunchentoot provides by _get-parameters*_ or _post-parameters*_. Becouse of using variable-names repetedly in execution process (e. g. recusive calls), stack can contain variable values several times. Parameters is used to provide variable values (see *WITH-VARIABLES*, *DEFINE-VARIABLE* and *PARAMETER-VALUE*).

*result* --- object that represents result of execution. Is contains view-name (by name-of), view-arguments (by arguments-of) and stack (by parameters-of).

__Description:__

Called to begin exection.



*Macro* __WITH-VIEW__

__Syntax:__

__with-view__ __(name &rest arguments &key &allow-other-keys) form* => result*__

__Arguments and Values:__

*name* --- view name.

*arguments* --- view arguments.

*forms* --- an implict progn.

*results* --- the values returned by the forms.

__Description:__

with-view dynamicaly bind view name and arguments. When value stack is exhusedthe view name and arguments are returned from nearest top with-view form.



*Macro* __WITH-COMMAND__

__Syntax:__

__with-command__ _(name) form* => result*_

__Arguments and Values:__

*name* --- symbol. Lexical variable name.

*forms* --- an implict progn.

*results* --- the values returned by the forms.

*Description:*

with-command creates new variable binding by using head of command-stack.



*Macro* __DISPATCH-COMMAND__

__Syntax:__

__dispatch-command__ _{normal-clause}* => result*_

_normal-clause ::= (keys form*)_

_results --- the values returned by the forms._

__Arguments and Values:__

*keys* --- a designator for a list of objects. In the case of case, the symbols t and otherwise may not be used as the keys designator. To refer to these symbols by themselves as keys, the designators (t) and (otherwise), respectively, must be used instead.

*forms* --- an implicit progn.

*results* --- the values returned by the forms in the matching clause.

__Description:__

'ecase' form for dispatching command. Same as:

```common-lisp

(with-command (command)

(ecase command (name-1 ...) ...))

```



*Macro* __WITH-VARIABLES__

__Syntax:__

__with-variables__ _({var | (var [values-form])}*) form* => result*_

__Arguments and Values:__

_var_ --- a symbol.

_values-form_ --- a form Result of form evolution must be a list.

_form_ --- a form.

_results_ --- the values returned by the forms.

__Description:__

with-variables create new variable bindings and execute a series of forms that use these bindings. For using variables they must be defined by DEFINE-VARIABLE.



*Macro* __DEFINE-VARIABLE__

__Syntax:__

__define-variable__ _(view-name var-name) form* => result_

__Arguments and Values:__

_view-name_ --- a symbol. Name of context's view (see WITH-VIEW).

_var-name_ --- a symbol. Name of variable (see WITH-VARIABLES).

__Description:__

Defines variable for use in WITH-VARIABLES.



*Function* __PARAMETER-VALUE__

__Syntax:__

__parameter-value__ _name => result_

__Arguments and Values:__

_name_ --- key of parameter is other-parameters passed to call-with-context. Function must called in DEFINE-VARIABLE body.

_result_ --- value of parameter.

__Description:__

Pop's value from stack.



*Function* __VARIANTS__

__Syntax:__

_variants_ __=> result__

__Arguments and Values:__

_result_ --- a list.

__Description:__

Return list of values defined by values-form in WITH-VARIABLE in context of a view (WITH-VIEW). Function must called in DEFINE-VARIABLE body.



*Function* __VIEW-OF__

__Syntax:__

__view-of__ _answer => result_

_answer_ --- object returned from CALL-WITH-CONTEXT.

_result_ --- a symbol.

__Description:__

Return current view name.



*Function* __ARGUMENTS-OF__

__Syntax:__

__arguments-of__ _answer => result_

_answer_ --- object returned from CALL-WITH-CONTEXT.

_result_ --- plist.

__Description:__

Return arguments of current view.



*Function* __PARAMETERS-OF__

__Syntax:__

__parameters-of__ _answer => result_

_answer_ --- object returned from CALL-WITH-CONTEXT.

_result_ --- alist.

__Description:__

Return current variables stack.



*Function* __PATH-OF__

__Syntax:__

__path-of__ _answer => result_

_answer_ --- object returned from CALL-WITH-CONTEXT.

_result_ --- a list.

__Description:__

Return current commands stack.



## Example:

