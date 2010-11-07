(in-package :clockwork)

;(deftype string-list () '(satisfies string-listp))
;(defun string-listp (arg)
;  (and (listp arg) (every #'stringp arg)))
;(deftype styles-list () '(satisfies styles-listp))
;(defun styles-listp (arg)
;  (and (listp arg)
;       (every (lambda (el) (member el :plaintext :html)) arg)))

(defclass reminder ()
  ((id) ;; all classes to be persisted with cl-prevalence need an id slot
   (emails :accessor reminder-emails
	   :initarg :emails
	   :type list)
   (title :accessor reminder-title
	  :initarg :title
	  :type string)
   (timestamp :accessor reminder-timestamp
	      :initarg :timestamp
	      :type timestamp)
   (summary :accessor reminder-summary
	    :initarg :summary
	    :type string)
   (at :accessor reminder-at
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
      (reminder-summary reminder))
    (delete-persistent-object-by-id *default-store*
	   'reminder (reminder-id reminder))))

(defmethod schedule-reminder ((reminder reminder))
  (let ((secs-until-reminder (round (local-time:timestamp-difference (reminder-at reminder) (now)))))
    (trivial-timers:schedule-timer
     (trivial-timers:make-timer (lambda ()
				  (send-and-delete reminder)))
     secs-until-reminder)))
