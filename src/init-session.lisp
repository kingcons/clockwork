(in-package :clockwork)

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (widget-children root)
	(make-reminder-form)))

(defmethod render-widget :before ((content form-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:div :id "site-banner"
	  (:img :src "/pub/images/clockwork_logo.jpg"))))

(defmethod render-widget :after ((content form-widget) &rest args)
  (declare (ignore args))
  (with-html
    (:div :id "main-content"
	  (:p :id "welcome"
	      "Welcome to Clockwork!")
	  (:p :id "instructions"
	      "Fill out and submit the form to get an email or text reminder.")
	  (:p :id "privacy"
	      "This site will not abuse your privacy or your data."))))

(defmethod render-page-body :after ((app clockwork) rendered-html)
  (with-html
    (:div :id "footer"
	  (:p :id "contact-info" "Please feel free to "
	      (:a :href "http://github.com/redline6561/clockwork/issues/" "Report Bugs") " or "
	      (:a :href "mailto:cl.ockwork.webdev@gmail.com" "Contact Us") " with any questions or comments.") (:br))))

(defun make-reminder-form ()
  (let ((reminder-form (make-instance 'form-widget :on-success 'submit-reminder-form)))
    (form-widget-initialize-from-view reminder-form 'reminder-form-view)
    reminder-form))

(defun display-overlay (content)
  (with-html
    (:div :id "overlay-content"
      (:img :src "/pub/images/clockwork_logo.jpg" :height "40%" :width "40%")
      (:h3 content)
      (send-script '(ps:chain ($jquery "#overlay-content")
		     (overlay
		      (ps:create top 400
		       load t)))))))

(defun submit-reminder-form (widget)
  (let ((new-reminder (create-reminder widget)))
    (schedule new-reminder)
    (persist-object *clockwork-store* new-reminder))
  (reset-form-widget widget))

(defun create-reminder (form-data)
  (with-form-values (subject message) form-data
    (let ((timestamps (get-timestamps form-data)))
      (make-instance 'reminder
		     :emails (get-emails form-data)
		     :title subject
		     :message message
		     :timestamp (first timestamps)
		     :at (second timestamps)))))
