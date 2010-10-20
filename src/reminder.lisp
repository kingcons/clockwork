(in-package :clockwork)

;(deftype string-list () '(satisfies string-listp))
;(defun string-listp (arg)
;  (and (listp arg) (every #'stringp arg)))
;(deftype styles-list () '(satisfies styles-listp))
;(defun styles-listp (arg)
;  (and (listp arg)
;       (every (lambda (el) (member el :plaintext :html)) arg)))

(defparameter *timezones*
  '(("UTC-12:00" . -43200) ; Eniwetok, Kwajalein
    ("UTC-11:00" . -39600) ; Midway Island, Samoa
    ("UTC-10:00" . -36000) ; Hawaii
    ("UTC-09:00" . -32400) ; Alaska
    ("UTC-08:00" . -28800) ; Pacific Time (US and Canada)
    ("UTC-07:00" . -25200) ; Mountain Time (US and Canada)
    ("UTC-06:00" . -21600) ; Central Time (US and Canada)
    ("UTC-05:00" . -18000) ; Eastern Time (US and Canada)
    ("UTC-04:00" . -14400) ; Atlantic Time (Canada), Caracas, La Paz
    ("UTC-03:30" . -12600) ; Newfoundland
    ("UTC-03:00" . -10800) ; Brazil, Buenos Aires, Georgetown
    ("UTC-02:00" . -7200) ; Mid-Atlantic
    ("UTC-01:00" . -3600) ; Azores, Cape Verde Islands
    ("UTC+00:00" . 0) ; London, Lisbon, Casablanca
    ("UTC+01:00" . 3600) ; Berlin, Brussels, Copenhagen, Madrid, Paris
    ("UTC+02:00" . 7200) ; Kaliningrad, South Africa
    ("UTC+03:00" . 10800) ; Baghdad, Riyadh, Moscow, St. Petersburg
    ("UTC+03:30" . 12600) ; Tehran
    ("UTC+04:00" . 14400) ; Abu Dhabi, Muscat, Baku, Tbilisi
    ("UTC+04:30" . 16200) ; Kabul
    ("UTC+05:00" . 18000) ; Ekaterinburg, Islamabad, Karachi, Tashkent
    ("UTC+05:30" . 19800) ; Bombay, Calcutta, Madras, New Delhi
    ("UTC+05:45" . 20700) ; Kathmandu
    ("UTC+06:00" . 21600) ; Almaty, Dhaka, Colombo
    ("UTC+07:00" . 25200) ; Bangkok, Hanoi, Jakarta
    ("UTC+08:00" . 28800) ; Beijing, Perth, Singapore, Hong Kong
    ("UTC+09:00" . 32400) ; Tokyo, Seoul, Osaka, Sapporo, Yakutsk
    ("UTC+09:30" . 34200) ; Adelaide, Darwin
    ("UTC+10:00" . 36000) ; Eastern Australia, Guam, Vladivostok
    ("UTC+11:00" . 39600) ; Magadan, Solomon Islands, New Caledonia
    ("UTC+12:00". 43200))) ; Auckland, Wellington, Fiji, Kamchatka

(defparameter *hour-choices*
  (loop for i from 0 to 23
     collecting `(,(format nil "~d" i) . ,i)))

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

(defview reminder-form-view (:type form :caption "Schedule an Event Reminder..."
			     :buttons '((:submit . "Submit")))
  (send-as :present-as (dropdown :choices '(("An email and a text." . :both)
					    ("Just an e-mail." . :email)
					    ("Just a text." . :text))
				 :welcome-name "How to send it")
	   :requiredp t)
  (email :requiredp nil)
  (cell-number :requiredp nil)
  (cell-carrier :present-as (dropdown :choices *sms-gateways*))
  (event-date :present-as (calendar) :requiredp t)
  (event-hour :present-as (dropdown :choices *hour-choices*))
  (event-minute :present-as (dropdown :choices   '(("00" . 0)
						   ("15" . 15)
						   ("30" . 30)
						   ("45" . 45))))
  (timezone :present-as (dropdown :choices *timezones*)
	    :requiredp t)
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
  (subject :requiredp t)
  (summary :requiredp nil :present-as (textarea :rows 5))
  (honeypot :requiredp nil :label "Leave this blank"))

(defun make-reminder-form ()
  (make-instance 'dataform
		 :data (make-instance (class-from-view 'reminder-form-view))
		 :ui-state :form
		 :form-view 'reminder-form-view))
