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

#|
example usage. put in a docstring?
(with-encrypted-smtp (:to "redline6561@gmail.com"
		      :subject "thesubject"
		      :style :html) ;; or :plaintext
  "a message to send. just any arbitrary old string. probably an accessor for a reminder's summary slot.")
|#
