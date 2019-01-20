;;;; solid-engine.asd

(asdf:defsystem #:solid-engine
  :description "User interface application controller"
  :author "Makarov Alexey <alexeys9@yandex.ru>"
  :license "LLGPL"
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:module "src"
			:depends-on ("package")
			:components ((:file "context")
				     (:file "commands" :depends-on ("context"))
				     (:file "views" :depends-on ("commands"))
				     (:file "variables" :depends-on ("views"))))))
