
(in-package :clockwork)

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (widget-children root)
	(make-reminder-form)))

(defun make-reminder-form ()
  (make-instance 'dataform
		 :data (make-instance (class-from-view 'reminder-form-view))
		 :ui-state :form
		 :form-view 'reminder-form-view
		 :on-success
		 (lambda (obj)
		   (let ((new-reminder (create-reminder (dataform-data obj))))
		     (schedule-reminder new-reminder)
		     (persist-object *default-store* new-reminder)))))

(defun create-reminder (form-data)
  (with-slots (subject summary) form-data
    (make-instance 'reminder
		   :emails (get-emails form-data)
		   :title subject
		   :summary summary
		   :timestamp (get-event-time form-data)
		   :at (get-reminder-time form-data))))
