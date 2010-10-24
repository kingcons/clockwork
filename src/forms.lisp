(in-package :clockwork)

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

(defview reminder-form-view (:type form :caption "Schedule an Event Reminder..."
			     :buttons '((:submit . "Submit")) :persistp nil)
  (send-as :present-as (dropdown :choices '(("An email and a text." . :both)
					    ("Just an e-mail." . :email)
					    ("Just a text." . :text))
				 :welcome-name "How to send it")
	   :requiredp t)
  (email :satisfies #'valid-email)
  (cell-number :satisfies #'valid-cell-number)
  (cell-carrier :present-as (dropdown :choices *sms-gateways*))
  (event-date :present-as (calendar) :parse-as (calendar))
;	      :requiredp t)
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
  (with-slots (send-as email cell-number cell-carrier) form-data
    (let ((sms-mail (concatenate 'string cell-number "@" cell-carrier)))
      ;; this was an ecase with keywords but the kws get converted to
      ;; strings somewhere in form submission
      (cond ((string= send-as "BOTH") (list email sms-mail))
	    ((string= send-as "EMAIL") (list email))
	    ((string= send-as "TEXT") (list sms-mail))))))

(defun get-timestamps (form-data)
  (with-slots (event-date event-hour event-minute
	       remind-me timezone) form-data
    (let* ((hour (parse-integer event-hour))
	   (minute (parse-integer event-minute))
	   (secs-before (parse-integer remind-me))
	   (timezone (parse-integer timezone))
	   (day (first event-date))
	   (month (second event-date))
	   (year (third event-date))
	   (event-time (encode-timestamp 0 0 minute hour day month year :offset timezone)))
      (list event-time
	    (timestamp- event-time remind-me :sec)))))
