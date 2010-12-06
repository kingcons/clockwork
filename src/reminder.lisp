(in-package :clockwork)

(defparameter *clockwork-host* nil)

(defparameter *timers* (make-hash-table)
  "A hash table that holds all scheduled timers keyed
by the reminder they trigger.")

(defparameter *unschedule-closures* (make-hash-table :test #'equal)
  "A hash table that holds closures which call unschedule
on a given reminder.")

(defclass reminder ()
  ((id :reader reminder-id) ;; classes to be persisted with the Store API need an id slot
   (emails :reader reminder-emails
	   :initarg :emails
	   :type list)
   (title :reader reminder-title
	  :initarg :title
	  :type string)
   (timestamp :reader reminder-timestamp
	      :initarg :timestamp
	      :type timestamp)
   (message :reader reminder-message
	    :initarg :message
	    :initform ""
	    :type string)
   (at :reader reminder-at
       :initarg :at
       :type timestamp)
   (hash :accessor reminder-hash
	 :type string)))

(defgeneric send-and-delete (reminder)
  (:documentation "Send the user their reminder then remove it from the datastore."))

(defmethod send-and-delete ((reminder reminder))
  (let ((id (reminder-id reminder)))
    (send-email :to (reminder-emails reminder)
		:subject (reminder-title reminder)
		:style :plain
		:body (reminder-message reminder))
    (remhash id *timers*)
    (remhash (reminder-hash reminder) *unschedule-closures*)
    (delete-persistent-object-by-id *clockwork-store* 'reminder id)))

(defgeneric schedule (reminder)
  (:documentation "Schedule the reminder to be sent at the time the user requested."))

(defmethod schedule ((reminder reminder))
  (let ((secs-until-reminder (round (timestamp-difference (reminder-at reminder) (now))))
	(timer (make-timer (lambda () (send-and-delete reminder))
			   :thread t
			   :name (reminder-id reminder))))
    (setf (gethash (reminder-id reminder) *timers*) timer)
    (schedule-timer timer secs-until-reminder)
    (send-unschedule-message reminder)))

(defgeneric send-unschedule-message (reminder)
  (:documentation "Create a link which unschedules the reminder and send an email with that link to the owner of the reminder."))

(defmethod send-unschedule-message ((reminder reminder))
  (let* ((link (make-unschedule-link reminder))
	 (email (first (reminder-emails reminder)))
	 (message-base "Your reminder has been scheduled. If you would like to unschedule it, simply visit the following link: "))
    (send-email :to email
		:subject (concatenate 'string "Clockwork Reminder: "
				      (reminder-title reminder))
		:style (if (sms-mail-p email) :plain :html)
		:body (if (sms-mail-p email)
			  (format nil "~A~A" message-base link)
			  (cl-who:with-html-output-to-string (html)
			    (:p message-base (:a :href link "Unschedule Reminder")))))))

(defgeneric make-unschedule-link (reminder)
  (:documentation "Create a link to unschedule the reminder from a hash of the emails, reminder-id and a salt.
Store the hash in the reminder and a closure to unschedule the reminder in a map keyed by the hash. Return the link."))

(defmethod make-unschedule-link ((reminder reminder))
  (let* ((hash (hash reminder))
	 (link (format nil "http://clockwork.~a/unschedule/~a" *clockwork-host* hash)))
    (setf (reminder-hash reminder) hash)
    (persist-object *clockwork-store* reminder)
    (setf (gethash hash *unschedule-closures*)
	  (lambda ()
	    (unschedule reminder)
	    (redirect (format nil "http://clockwork.~a/" *clockwork-host*))
	    (display-overlay "Thank you for using Clockwork.")))
    link))

(defgeneric hash (reminder)
  (:documentation "Creates a SHA-1 hash from a reminder's email(s), ID and a salt."))

(defmethod hash ((reminder reminder))
  (let ((string-list (append (reminder-emails reminder)
			     `(,(format nil "~A" (reminder-id reminder)))
			     `(,(make-salt)))))
    (hash-concat string-list)))

(defgeneric unschedule (reminder)
  (:documentation "Unschedule the timer associated with the reminder and remove it from the datastore,
then redirect the user and thank them for "))

(defmethod unschedule ((reminder reminder))
  (let ((id (reminder-id reminder)))
    (trivial-timers:unschedule-timer (gethash id *timers*))
    (remhash id *timers*)
    (delete-persistent-object-by-id *clockwork-store* 'reminder id))
  (remhash (reminder-hash reminder) *unschedule-closures*))

(defun recover-reminders ()
  "A function to reschedule reminders after a reboot. Based on testing,
any that expired during the reboot will be sent when the schedule method is called.
Better late than never, right?"
  (mapcar #'schedule (find-persistent-objects *clockwork-store* 'reminder)))
