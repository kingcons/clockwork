(defpackage #:clockwork
  (:use :cl :weblocks :local-time
        :f-underscore :anaphora)
  (:import-from :hunchentoot
		#:header-in
		#:set-cookie
		#:set-cookie*
		#:cookie-in
		#:user-agent
		#:referer)
  (:import-from :split-sequence
		#:split-sequence)
  (:import-from :trivial-timers
		#:make-timer
		#:schedule-timer)
  (:export #:start-clockwork
	   #:stop-clockwork
	   #:recover-reminders)
  (:documentation
   "A web application based on Weblocks."))

(in-package :clockwork)

;; A macro that generates a class or this webapp

(defwebapp clockwork
    :prefix "/"
    :description "Fire-and-Forget Event Reminders"
    :init-user-session 'clockwork::init-user-session
    :autostart nil                   ;; have to start the app manually
    :ignore-default-dependencies nil ;; accept the defaults
    :hostnames '("clockwork.redlinernotes.com")
    :dependencies '((:stylesheet "jquery-ui")
		    (:stylesheet "clockwork")
		    (:script "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js")
		    (:script "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/jquery-ui.min.js")
		    (:script "http://cdn.jquerytools.org/1.2.5/tiny/jquery.tools.min.js")
		    (:javascript-code "var $jquery = jQuery.noConflict();"))
    :debug t
    )

;; Top level start & stop scripts

(defun start-clockwork (&rest args)
  "Starts the application by calling 'start-weblocks' with appropriate
arguments."
  (apply #'start-weblocks args)
  (start-webapp 'clockwork))

(defun stop-clockwork ()
  "Stops the application by calling 'stop-weblocks'."
  (stop-webapp 'clockwork)
  (stop-weblocks))
