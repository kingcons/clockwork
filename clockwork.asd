;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:clockwork-asd
  (:use :cl :asdf))

(in-package :clockwork-asd)

(defsystem clockwork
    :name "clockwork"
    :version "0.0.1"
    :maintainer "Brit Butler"
    :author "Brit Butler"
    :licence "LLGPL"
    :description "A yourli.st clone"
    :depends-on (:weblocks :cl-smtp :local-time :trivial-timers :split-sequence)
    :components ((:file "clockwork")
		 (:module conf
		  :components ((:file "stores")
			       (:file "confidential"))
		  :depends-on ("clockwork"))
		 (:module src
		  :components ((:file "calendar")
			       (:file "messaging")
			       (:file "reminder"
				      :depends-on ("messaging"))
			       (:file "forms"
				      :depends-on ("messaging" "calendar"))
			       (:file "init-session"
				      :depends-on ("reminder" "forms")))
		  :depends-on ("clockwork" conf))))
