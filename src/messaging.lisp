(in-package :clockwork)

(defparameter *mail-server* "smtp.gmail.com")

(defun send-email (&key to subject (style :plain)
		  (from *smtp-user*) body)
  (assert (and to subject body))
  (cl-smtp:send-email *mail-server* from to subject body
		      :ssl :tls
		      :authentication `(,*smtp-user* ,*smtp-pass*)
		      :extra-headers (when (eql style :html)
				       '(("Content-type"
					  "text/html; charset=\"iso-8859-1\"")))))

(defparameter *sms-gateways*
  ;; list is derived from http://en.wikipedia.org/wiki/List_of_SMS_gateways
  '(("AT&T/Cingular" . "txt.att.net")
    ("Alltel" . "text.wireless.alltel.com")
    ("Boost Mobile" . "myboostmobile.com")
    ("Cincinatti Wireless" . "gocbw.com")
    ("MetroPCS" . "mymetropcs.com")
    ("Sprint/PCS" . "messaging.sprintpcs.com")
    ("Sprint/Nextel" ."page.nextel.com")
    ("T-Mobile" . "tmomail.net")
    ("US Cellular" . "email.uscc.net")
    ("Verizon" . "vtext.com")
    ("Virgin Mobile" . "vmobl.com")))

(defun sms-mail-p (email)
  (let ((domain (second (split-sequence #\@ email))))
    (member domain *sms-gateways* :key #'cdr :test #'equal)))
