(in-package :clockwork)

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (widget-children root)
	(make-reminder-form)))

(defun make-reminder-form ()
  (let ((reminder-form (make-instance 'form-widget :on-success 'submit-reminder-form)))
    (form-widget-initialize-from-view reminder-form 'reminder-form-view)
    reminder-form))

(defun submit-reminder-form (widget)
  (let ((new-reminder (create-reminder widget)))
    (schedule-reminder new-reminder)
    (persist-object *default-store* new-reminder)))

(defun create-reminder (form-data)
  (with-form-values (subject summary) form-data
    (let ((timestamps (get-timestamps form-data)))
      (make-instance 'reminder
		     :emails (get-emails form-data)
		     :title subject
		     :summary summary
		     :timestamp (first timestamps)
		     :at (second timestamps)))))
