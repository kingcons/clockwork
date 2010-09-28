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
