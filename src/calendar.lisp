(in-package :clockwork)

;; calendar presentation
(defclass calendar-presentation (input-presentation)
  ())

;; calendar parser
(defclass calendar-parser (text-parser)
  ())

(defmethod parse-view-field-value ((parser calendar-parser) value obj
                                   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (values t (text-input-present-p value) value))

;; calendar form-widget code
(define-widget calendar-field-widget (field-widget)
  ()
  (:default-initargs :parser (lambda (raw-value)
			       (values t raw-value))))

(defmethod field-presentation->field-widget-class ((presentation calendar-presentation))
  'calendar-field-widget)

(defmethod render-field-contents ((form form-widget) (field calendar-field-widget))
  (with-html
    (:input :type "hidden" :name (name-of field) :value (datestring))
    (:div :id "datepicker"
	  (send-script
	   (ps:ps
	     ($jquery (lambda ()
			(ps:chain ($jquery "#datepicker")
				  (datepicker
				   (ps:create date-format "dd-mm-yy"
					      on-select (lambda (date inst)
							  (ps:chain ($jquery "[name=event-date]")
								    (val date)))))))))))))

(defun datestring ()
  (subseq (format-timestring nil (now) :format '(:day "-" :month "-" :year)) 0 10))