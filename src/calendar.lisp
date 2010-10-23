(in-package :clockwork)

;; The following code largely derived from nunb's YUI Calendar Presentation

;; calendar
(defclass calendar-presentation (input-presentation)
  ())

(defmethod render-view-field-value (value (presentation calendar-presentation)
                                    field view widget obj &rest args)
  (declare (ignore args))
  (with-html
    (:span value)))

(defmethod render-view-field-value  (value (presentation calendar-presentation)
                                    (field form-view-field) (view form-view) widget obj
                                    &rest args &key intermediate-values &allow-other-keys)
  (declare (ignore args intermediate-values))
  (let ((attrib-name (attributize-name (view-field-slot-name field))))
    (with-html
      (:div :id attrib-name
	    (send-script
	     (ps:ps ($jquery (lambda ()
			 (ps:chain ($jquery "#event-date") (datepicker))))))))))

;; WARNING: EVIL HACK TERRITORY!
;; Note that we had to reverse the order of render-page-headers and the mapc call
;; in weblocks render-page function for this to work.
(defmethod render-page-headers ((app clockwork))
   (with-html
     (:script "var $jquery = jQuery.noConflict();")))

;; parser
(defclass calendar-parser (text-parser)
  ())

(defmethod parse-view-field-value ((parser calendar-parser) value obj
                                   (view form-view) (field form-view-field) &rest args)
  (declare (ignore args))
  (if (text-parser-matches parser)
      (when (ppcre:all-matches (text-parser-matches parser) value)
        (values t (text-input-present-p value) (alex-date-string-yui value)))
      (values t (text-input-present-p value) (alex-date-string-yui value) )))

(defun alex-date-string-yui   (yuistr)
  "Takes yui calendar string and normalizes it"
  (multiple-value-bind (lst x)
      (read-date-from yuistr (the-year))
    (let ((day (nth 1 lst))
	    (month (nth 0 lst))
	    (year  (nth 2 lst)))
      (format nil "~2,'0d/~2,'0D/~2,'0D"  day month (last-two year) ))))

; Parser from metatilities, adjusted to use :d :m :y
(defun read-date-from (string &optional default-year)
  "strips the date signature off of the front of string. can handle slash-delimited format:
[M]M/[D]D[/YY] -- changed to d/m/y "
  (let* ((end (or (position #\space string) (length string)))
         (sbuff (subseq string 0 (min (1+ end) (length string))))
         la da mo yr (seq '(:d :m :y)))
    (loop until (string= sbuff "") do
          (setq la (or (position #\/ sbuff) (length sbuff)))
          (case (pop seq)
	        (:y (setq yr (read-from-string (subseq sbuff 0 la))))
		    (:d (setq da (read-from-string (subseq sbuff 0 la))))
		        (:m (setq mo (read-from-string (subseq sbuff 0 la))))
            (t  (error "unrecognized time format in ~S" string)))
          (setq sbuff (subseq sbuff (min (1+ la) (length sbuff)) (length sbuff))))
    (when (and yr (< yr 100)) (incf yr 1900))
    (unless (and da mo)
      (error "incomplete date format in ~S" string))
    (values (list mo da (or yr default-year))
            end)))
