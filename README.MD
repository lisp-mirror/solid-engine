# Solid-engine - The Common Lisp MVC stack-based controller

Solid-engine is a stack-based controller. The idea is bored from
SICP. As opposed to amb-operator where selection of values is fully
non-deterministic, in solid-engine selection of action (command) and
values is determined by user. In point of program view, selection also
non-deterministic.

Solid-engine is developed for use with HTTP-protocol and fits for use
with stateless-protocols. It view- and server-agnostic. You can freely
choose (web-)server and presentation layer by implementing thin
integration code.

## Concept, problem and implementation

If we imagine user's work on site as process, then we can code user's
work logic as program code with points of choice in some places. In
that places we have information about objects that user must select
(from set of objects) or provide (simple type values) to continue
execution. Each choice point is place where user can do choice. Each
choice point is "view" in MVC pattern.

Other problem is using recover state when using stateless-protocols
(e.g. HTTP). Soling-engine solve this problem by runnng program from
begining and continue execution using choice-stack in choice points.
It means that each request must provide choice stack. Execution ends
in choice point when stack does not have parameters for continue and
process of execution program return information about progress of
execution (name of view, stack of parameters). From example, this
information can be used to genereate page for user and send
response. Further request must send this choice-stack with additional
parameters in head. Choice stack can be stored in URL (as query
parameters and parts of path) and POST-parameters.

## Usage

### Integration

First of all, you must integrate it with your web-server and
presentation-layer.

For Hunchentoot and cl-closure-tmplates integration looks like:

...code

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

#:parameter-name-of
#:parameter-value

## Example:

