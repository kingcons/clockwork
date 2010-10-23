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
  (:export #:start-clockwork
	   #:stop-clockwork)
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
		    (:script "jquery.min")
		    (:script "jquery-ui.min"))
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
