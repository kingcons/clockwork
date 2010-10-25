(in-package :clockwork)

;; calendar form-widget code
(define-widget calendar-field-widget (field-widget)
  ()
  (:default-initargs :parser (lambda (raw-value)
			       (values t raw-value))))

(defmethod field-presentation->field-widget-class ((presentation calendar-presentation))
  'calendar-field-widget)

(defmethod render-field-contents ((form form-widget) (field calendar-field-widget))
  (with-html
    (:input :type "hidden" :id (name-of field))
    (:div :id "datepicker"
	  (send-script
	   (ps:ps
	     ($jquery (lambda ()
			(ps:chain ($jquery "#datepicker")
				  (datepicker
				   (ps:create date-format "dd-mm-yyyy"
					      on-select (lambda (date obj)
							  (ps:chain ($jquery "#event-date")
								    (val date)))))))))))))

;; calendar presentation
(defclass calendar-presentation (input-presentation)
  ())

;; calendar parser
(defclass calendar-parser (text-parser)
  ())

(defmethod parse-view-field-value ((parser calendar-parser) value obj
                                   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (setf *calendar-val* (list value field (text-input-present-p value) obj))
  (values t (text-input-present-p value) value))