(in-package :clockwork)

(defparameter *timezones*
  '(("UTC-12:00 (Eniwetok, Kwajalein)" . -43200)
    ("UTC-11:00 (Midway Island, Samoa)" . -39600)
    ("UTC-10:00 (Hawaii)" . -36000)
    ("UTC-09:00 (Alaska)" . -32400)
    ("UTC-08:00 (Pacific Time)" . -28800)
    ("UTC-07:00 (Mountain Time)" . -25200)
    ("UTC-06:00 (Central Time)" . -21600)
    ("UTC-05:00 (Eastern Time)" . -18000)
    ("UTC-04:00 (Atlantic Time, Caracas)" . -14400)
    ("UTC-03:30 (Newfoundland)" . -12600)
    ("UTC-03:00 (Brazil, Buenos Aires, Georgetown)" . -10800)
    ("UTC-02:00 (Mid-Atlantic)" . -7200)
    ("UTC-01:00 (Azores, Cape Verde Islands)" . -3600)
    ("UTC+00:00 (Lisbon, London, Casablanca)" . 0)
    ("UTC+01:00 (Berlin, Brussels, Copenhagen, Madrid, Paris)" . 3600)
    ("UTC+02:00 (Kaliningrad, South Africa)" . 7200)
    ("UTC+03:00 (Baghdad, Moscow, Riyadh, St. Petersburg)" . 10800)
    ("UTC+03:30 (Tehran)" . 12600)
    ("UTC+04:00 (Abu Dhabi, Baku, Muscat, Tbilisi)" . 14400)
    ("UTC+04:30 (Kabul)" . 16200)
    ("UTC+05:00 (Ekaterinburg, Islamabad, Karachi, Tashkent)" . 18000)
    ("UTC+05:30 (Bombay, Calcutta, Madras, New Delhi)" . 19800)
    ("UTC+05:45 (Kathmandu)" . 20700)
    ("UTC+06:00 (Almaty, Colombo, Dhaka)" . 21600)
    ("UTC+07:00 (Bangkok, Hanoi, Jakarta)" . 25200)
    ("UTC+08:00 (Beijing, Hong Kong, Perth, Singapore)" . 28800)
    ("UTC+09:00 (Osaka, Seoul, Sapporo, Tokyo, Yakutsk)" . 32400)
    ("UTC+09:30 (Adelaide, Darwin)" . 34200)
    ("UTC+10:00 (Eastern Australia, Guam, Vladivostok)" . 36000)
    ("UTC+11:00 (Magadan, New Caledonia, Solomon Islands)" . 39600)
    ("UTC+12:00 (Auckland, Fiji, Kamchatka, Wellington)". 43200)))

(defparameter *hour-choices*
  (loop for i from 0 to 23
     collecting `(,(format nil "~d" i) . ,i)))

(defview reminder-form-view (:type form :caption "Schedule an Event Reminder..."
			     :buttons '((:submit . "Submit")) :persistp nil)
  (send-as :present-as (dropdown :choices '(("An email and a text." . :both)
					    ("Just an e-mail." . :email)
					    ("Just a text." . :text))
				 :welcome-name "How to send it")
	   :requiredp t)
  (email :satisfies 'valid-email)
  (cell-number :satisfies 'valid-cell-number)
  (cell-carrier :present-as (dropdown :choices *sms-gateways*))
  (event-date :present-as (calendar) :requiredp t)
  (event-hour :present-as (dropdown :choices *hour-choices*)
	      :requiredp t)
  (event-minute :present-as (dropdown :choices   '(("00" . 0)
						   ("15" . 15)
						   ("30" . 30)
						   ("45" . 45)))
		:requiredp t)
  (timezone :present-as (dropdown :choices *timezones*)
	    :requiredp t)
  (remind-me :present-as (dropdown :choices '(("At the event" . 0)
					      ("5 minutes before" . 300)
					      ("10 minutes before" . 600)
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
  (summary :present-as (textarea :rows 5))
  (honeypot :label "Leave this blank" :satisfies #'null))

(defun valid-email (user-input)
  "Ensure that there is an @ and a . and input not containing @s before and after each."
  (or (cl-ppcre:scan "^[^@]+@[^@]+\\.[^@]+$" user-input)
      (values nil "Your email must have an @, a . and text before and after both.")))

(defun valid-cell-number (user-input)
  "Ensure that only numbers are given and there are at least 10."
  (or (cl-ppcre:scan "^[0-9]{10,}$" user-input)
      (values nil "Your number must have only numbers and at least 10 of them.")))

(defun get-emails (form-data)
  (with-form-values (send-as email cell-number cell-carrier) form-data
    (let ((sms-mail (concatenate 'string cell-number "@" cell-carrier)))
      ;; this was an ecase with keywords but weblocks converts
      ;; the keywords to strings somewhere in form submission
      (cond ((string= send-as "BOTH") (list email sms-mail))
	    ((string= send-as "EMAIL") (list email))
	    ((string= send-as "TEXT") (list sms-mail))))))

(defun get-timestamps (form-data)
  (with-form-values (event-date event-hour event-minute
		     remind-me timezone) form-data
    (let* ((hour (parse-integer event-hour))
	   (minute (parse-integer event-minute))
	   (reminder-time-period (parse-integer remind-me))
	   (timezone (parse-integer timezone))
	   (datestring (split-sequence #\- event-date))
	   (day (parse-integer (first datestring)))
	   (month (parse-integer (second datestring)))
	   (year (parse-integer (third datestring)))
	   (event-time (encode-timestamp 0 0 minute hour day month year :offset timezone)))
      (list event-time
	    (timestamp- event-time reminder-time-period :sec)))))
