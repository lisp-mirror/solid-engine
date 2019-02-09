;;;; solid-engine.asd

(asdf:defsystem #:solid-engine
  :description "The Common Lisp stack-based application controller"
  :author "Makarov Alexey <alexeys9@yandex.ru>"
  :license "MIT"
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:module "src"
			:depends-on ("package")
			:components ((:file "context")
				     (:file "commands" :depends-on ("context"))
				     (:file "views" :depends-on ("commands"))
				     (:file "variables" :depends-on ("views"))))))
