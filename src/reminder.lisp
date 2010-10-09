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
  (with-slots (emails title summary) reminder
    (loop for email in emails do
	 (with-encrypted-smtp (:to email :subject title
			       :style (if (sms-mail-p email)
					  :plain
					  :html))
	   summary)
	 (delete-persistent-object-by-id *default-store* 'reminder
					 (object-id email)))))

(defwidget reminder-form ()
  ((send-as :accessor send-as
	    :type keyword)
   (email :accessor email
	  :type string)
   (cell-number :accessor cell-number
		:type string)
   (cell-carrier :accessor cell-carrier
		 :type string)
   (subject :accessor subject
	    :type string)
   (summary :accessor summary
	    :type string)
   (event-date :accessor event-date
	       :type string)
   (event-time :accessor event-time
	       :type string)
   (remind-me :accessor remind-me
	      :type string)
   (timezone :accessor timezone
	     :type string)
   (honeypot :accessor honeypot
	     :type string)))

(defmethod render-widget-body ((widget reminder-form) &rest args)
  (render-object-view widget 'reminder-form-view))

(defview reminder-form-view (:type form :caption "Schedule an Event Reminder...")
  (send-as :present-as (dropdown :choices '(("An email and a text." . :both)
					    ("Just an e-mail." . :email)
					    ("Just a text." . :text))
				 :welcome-name "How to send it")
	   :requiredp t)
  (email :requiredp nil)
  (cell-number :requiredp nil)
  (cell-carrier :present-as (dropdown :choices *sms-gateways*))
  (event-date :present-as date-entry
	      :requiredp t) ; :label ""?
  (remind-me :present-as (dropdown :choices '(("At the event" . 0)
					      ("15 minutes before" . 900)
					      ("30 minutes before" . 1800)
					      ("45 minutes before" . 2700)
					      ("1 hour before" . 3600)
					      ("2 hours before" . 7200)
					      ("1 day before" . 86400)
					      ("2 days before" . 172800)
					      ("1 week before" . 604800)
					      ("2 weeks before" . 1209600)))
	     :requiredp t)
  (timezone :present-as dropdown
	    :requiredp t)
  (subject :requiredp t)
  (summary :requiredp nil :present-as (textarea :rows 5))
  (honeypot :requiredp nil))
