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
  ;(display-overlay "Thanks for your submission.")
  (let ((new-reminder (create-reminder widget)))
    (schedule new-reminder)
    (persist-object *clockwork-store* new-reminder))
  (reset-form-widget widget))



(defun create-reminder (form-data)
  (with-form-values (subject summary) form-data
    (let ((timestamps (get-timestamps form-data)))
      (make-instance 'reminder
		     :emails (get-emails form-data)
		     :title subject
		     :summary summary
		     :timestamp (first timestamps)
		     :at (second timestamps)))))
