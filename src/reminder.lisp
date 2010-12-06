(in-package :clockwork)

;(deftype string-list () '(satisfies string-listp))
;(defun string-listp (arg)
;  (and (listp arg) (every #'stringp arg)))
;(deftype styles-list () '(satisfies styles-listp))
;(defun styles-listp (arg)
;  (and (listp arg)
;       (every (lambda (el) (member el :plaintext :html)) arg)))

(defparameter *timers* (make-hash-table)
  "A hash table that holds all scheduled timers keyed
by the reminder they trigger.")

(defclass reminder ()
  ((id :reader reminder-id) ;; all classes to be persisted with cl-prevalence need an id slot
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

(defgeneric send-and-delete (reminder)
  (:documentation "Send the user their reminder as requested and then remove it from the datastore."))

(defmethod send-and-delete ((reminder reminder))
  (loop for email in (reminder-emails reminder) do
    (with-encrypted-smtp (:to email :subject (reminder-title reminder)
			      :style (if (sms-mail-p email)
					 :plain
					 :html))
      (reminder-message reminder)))
  (delete-persistent-object-by-id *clockwork-store* 'reminder (reminder-id reminder)))

(defgeneric schedule (reminder)
  (:documentation "Schedule the reminder to be sent at the time the user requested."))

(defmethod schedule ((reminder reminder))
  (let ((secs-until-reminder (round (timestamp-difference (reminder-at reminder) (now))))
	(timer (make-timer (lambda () (send-and-delete reminder))
			   :thread t
			   :name (reminder-id reminder))))
    (setf (gethash (reminder-id reminder) *timers*) timer)
    (schedule-timer timer secs-until-reminder)))

(defgeneric cancel (reminder)
  (:documentation "Unschedule the timer associated with the reminder then remove it from the datastore."))

(defmethod cancel ((reminder reminder))
  (let ((id (reminder-id reminder)))
    (trivial-timers:unschedule-timer (gethash id *timers*))
    (delete-persistent-object-by-id *clockwork-store* 'reminder id)))

(defun recover-reminders ()
  "A function to reschedule reminders after a reboot. Based on testing,
any that expired during the reboot will be sent when the schedule method is called.
Better late than never, right?"
  (mapcar #'schedule (find-persistent-objects *clockwork-store* 'reminder)))
