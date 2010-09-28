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
    :depends-on (:weblocks :cl-smtp :local-time :trivial-timers)
    :components ((:file "clockwork")
		 (:module conf
		  :components ((:file "stores")
			       (:file "confidential"))
		  :depends-on ("clockwork"))
		 (:module src
		  :components ((:file "init-session")
			       (:file "messaging")
			       (:file "reminder"))
		  :depends-on ("clockwork" conf))))

