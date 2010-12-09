(in-package :clockwork)

(defparameter *clockwork-host* nil
  "A string representing the domain that clockwork is a subdomain of.")

(defparameter *timers* (make-hash-table)
  "A hash table that holds all scheduled timers keyed by their reminder's id.")

(defparameter *reminders* (make-hash-table :test #'equal)
  "A hash table that holds reminders keyed by their hash.")

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
       :type timestamp)))

(defgeneric send-reminder (reminder &key delete-p)
  (:documentation "Send the reminder. If delete-p is true, delete the reminder afterwards."))

(defmethod send-reminder ((reminder reminder) &key delete-p)
  (send-email :to (reminder-emails reminder)
	      :subject (reminder-title reminder)
	      :style :plain
	      :body (reminder-message reminder))
  (when delete-p
    (delete-reminder (reminder-id reminder))))

(defun delete-reminder (id &key unschedule-p)
  "Delete the reminder and its timer. If unschedule-p is true, unschedule the timer first."
  (when unschedule-p
    (trivial-timers:unschedule-timer (gethash id *timers*)))
  (remhash id *timers*)
  (delete-persistent-object-by-id *clockwork-store* 'reminder id))

(defgeneric schedule-reminder (reminder)
  (:documentation "Schedule the reminder to be sent at the time the user requested."))

(defmethod schedule-reminder ((reminder reminder))
  (let ((secs-until-reminder (round (timestamp-difference (reminder-at reminder) (now))))
	(timer (make-timer (lambda () (send-reminder reminder :delete-p t))
			   :thread t
			   :name (reminder-id reminder))))
    (setf (gethash (reminder-id reminder) *timers*) timer)
    (schedule-timer timer secs-until-reminder)
    (send-unschedule-message reminder)))

(defgeneric send-unschedule-message (reminder)
  (:documentation "Create a link which unschedules the reminder and send an email
with that link to the owner of the reminder."))

(defmethod send-unschedule-message ((reminder reminder))
  (let* ((link (make-unschedule-link reminder))
	 (email (first (reminder-emails reminder)))
	 (message-base "Your reminder has been scheduled.
If you would like to unschedule it, simply visit the following link: "))
    (send-email :to email
		:subject (format nil "Clockwork Reminder: ~A" (reminder-title reminder))
		:style (if (sms-mail-p email) :plain :html)
		:body (if (sms-mail-p email)
			  (format nil "~A~A" message-base link)
			  (cl-who:with-html-output-to-string (html)
			    (:p (cl-who:esc message-base)
				(:a :href link "Unschedule Reminder") (:br) (:br)
				(:i "Thanks for using clockwork!")))))))

(defun make-unschedule-link (reminder)
  "Hash the reminder and create a link from the hash which will unschedule the reminder.
Store the reminder in a map keyed by the hash. Return the link."
  (check-type reminder reminder)
  (let* ((hash (hash reminder))
	 (link (format nil "http://clockwork.~a/unschedule/~a" *clockwork-host* hash)))
    (setf (gethash hash *reminders*) reminder)
    link))

(defun hash (reminder)
  "Creates a SHA-1 hash from a reminder's email(s), ID and a salt."
  (check-type reminder reminder)
  (let ((string-list (append (reminder-emails reminder)
			     `(,(format nil "~A" (reminder-id reminder)))
			     `(,(make-salt)))))
    (hash-concat string-list)))

(defun recover-reminders ()
  "A function to reschedule reminders after a reboot. Any that expired during
the reboot will be sent when the schedule method is called. Better late than never."
  (mapcar #'schedule-reminder (find-persistent-objects *clockwork-store* 'reminder)))
