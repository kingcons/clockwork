(in-package :clockwork)

(defconstant +mail-server+ "smtp.gmail.com")

(defmacro with-encrypted-smtp ((&key to subject style
			       (from "cl.ockwork.webdev@gmail.com"))
			       &body body)
  `(cl-smtp:send-email ,+mail-server+ ,from ,to ,subject
		       (if (eql ,style :html)
			   (with-html ,@body) ;; TODO: make a nicer render style
			   ,@body)
		       ;; it's worth noting send-email takes a :cc argument
		       :ssl :tls
		       :authentication '(,*smtp-user* ,*smtp-pass*)
		       ,@(when (eql style :html)
			       '(:extra-headers
				 '(("Content-type"
				    "text/html; charset=\"iso-8859-1\""))))))

(defvar *sms-gateways*
  ;; list is derived from http://en.wikipedia.org/wiki/List_of_SMS_gateways
  '(("AT&T/Cingular" . "txt.att.net")
    ("Boost Mobile" . "myboostmobile.com")
    ("MetroPCS" . "mymetropcs.com")
    ("Sprint/PCS" . "messaging.sprintpcs.com")
    ("Sprint/Nextel" ."page.nextel.com")
    ("T-Mobile" . "tmomail.net")
    ("Verizon" . "vtext.com")
    ("Virgin Mobile" . "vmobl.com")))

(defun sms-mail-p (email)
  (let ((domain (second (split-sequence #\@ email))))
    (member domain *sms-gateways* :key #'cdr :test #'equal)))
