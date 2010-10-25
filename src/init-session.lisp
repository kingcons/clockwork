(in-package :clockwork)

;; Define callback function to initialize new sessions
(defun init-user-session (root)
  (setf (widget-children root)
	(make-reminder-form)))

;; new make-reminder-form
(defun make-reminder-form ()
  (let ((reminder-form (make-instance 'form-widget)))
;				      :on-success #'submit-reminder-form)))
    (form-widget-initialize-from-view reminder-form 'reminder-form-view)))

;; (defun make-reminder-form ()
;;   (make-instance 'dataform
;; 		 :data (make-instance (class-from-view 'reminder-form-view))
;; 		 :ui-state :form
;; 		 :form-view 'reminder-form-view
;; 		 :on-success (lambda (obj)
;; 			       (setf *last-post* (dataform-data obj)))))
		 ;; (lambda (obj)
		 ;;   (let ((new-reminder (create-reminder (dataform-data obj))))
		 ;;     (schedule-reminder new-reminder)
		 ;;     (persist-object *default-store* new-reminder)))))

(defun submit-reminder-form (obj)
  (setf *last-post* obj))

(defun create-reminder (form-data)
  (with-slots (subject summary) form-data
    (let ((timestamps (get-timestamps form-data)))
      (make-instance 'reminder
		     :emails (get-emails form-data)
		     :title subject
		     :summary summary
		     :timestamp (first timestamps)
		     :at (second timestamps)))))
