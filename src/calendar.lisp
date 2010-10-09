(in-package :clockwork)

;; Originally written by Aaron Feng using cl-date-calc:
;; See http://common-lisp.net/project/cl-date-calc/
;; Ported to local-time by Brit Butler 2010/10/09
;; See http://common-lisp.net/project/local-time/manual.html

;; % cat model/calendar.lisp
(defparameter *month-names*
  '("" "January" "February" "March" "April" "May" "June"
    "July" "August" "September" "October" "November" "December"))

(defun todayp (year month day)
  (let ((today (now)))
    (and (= (timestamp-year today) year)
	 (= (timestamp-month today) month)
	 (= (timestamp-day today) day))))

(defun current-day ()
  (timestamp-day (now)))

(defun current-month ()
  (timestamp-month (now)))

(defun current-year ()
  (timestamp-year (now)))

(defun day-of-month-name (month year day)
  (let ((day-number (timestamp-day-of-week
		     (encode-timestamp 0 0 0 0 day month year))))
    (if (= day-number 0)
	7
	day-number)))

(defun adjust-month (year month adjustment)
  (let ((adjusted-timestamp (timestamp+
			     (encode-timestamp 0 0 0 0 1 month year)
			     adjustment :month)))
    (values (timestamp-year adjusted-timestamp)
	    (timestamp-month adjusted-timestamp))))

(defun nth-month-name (month)
    (nth month *month-names*))

(defun calendar-offset (dow)
  (if (= dow 7) -1
      (* -1 (1+ dow))))

(defun previous-month-offset (dow)
  (cond ((>= dow 7) 0)
         (t dow)))

(defun next-month-offset (dow)
  (let ((map '((7 . 6) (1 . 5) (2 . 4)
               (3 . 3) (4 . 2) (5 . 1)
               (6 .  0))))
    (cdr (assoc dow map))))

(defun number-of-weeks (year month)
  (let* ((month-days (days-in-month month year))
	 (first-day-of-month
	  (day-of-month-name month year 1))
	 (last-day-of-month
	  (day-of-month-name month year month-days)))
    (/ (+ (previous-month-days first-day-of-month)
	  (next-month-offset last-day-of-month)
	  month-days) 7)))

;;; November 2008
;;; '(((2008 . 10) . 26 27 28 29 30 31)
;;;   ((2008 . 11 ). 1 2 3 4 5 6 7 8
;;;                  9 10 11 12 13 14 15
;;;                  16 17 18 19 20 21 22
;;;                  23 24 25 26 27 28 29
;;;                  30)
;;;   ((2008 . 12) . 1 2 3 4 5 6))
(defun previous-month-days (year month)
  (let* ((first-dow
	  (day-of-month-name month year 1))
         (day-offset (calendar-offset first-dow)))
    (cons (multiple-value-bind (year month)
	      (adjust-month year month -1)
	    (cons year month))
          (loop repeat (previous-month-offset first-dow) do
	       (incf day-offset)
	     collect (timestamp-day
		      (timestamp+ (encode-timestamp 0 0 0 0 1 month year)
				  day-offset :day))))))

(defun current-month-days (year month)
  (let ((first-dow
	 (day-of-month-name month year 1))
         (day-offset 0))
    (cons (cons year month)
          (loop repeat (days-in-month month year)
            collect (incf day-offset)))))

(defun next-month-days (year month)
  (let* ((month-days (days-in-month month year))
	 (last-dow
	  (day-of-month-name month year month-days))
         (day-offset 0))
    (cons (multiple-value-bind (year month)
                               (adjust-month year month 1)
                               (cons year month))
          (loop repeat (next-month-offset last-dow)
            collect (incf day-offset)))))

(defun build-calendar (year month)
  `(,(previous-month-days year month)
    ,(current-month-days year month)
    ,(next-month-days year month)))


;; % cat ui/calendar.lisp

(defwidget calendar-widget ()
  ((display-month
     :type integer
     :accessor display-month
     :initarg :month
     :initform (current-month))
   (display-year
     :type integer
     :accessor display-year
     :initarg :year
     :initform (current-year))
   (selected-date
     :accessor selected-date
     :initarg :selected-date
     :initform (list (current-day) (current-month) (current-year)))))

(defun create-month-nav-link (name widget adjustment)
  (render-link (f_% (multiple-value-bind (year month)
                      (adjust-month (display-year widget) (display-month widget) adjustment)
                      (setf (display-year widget) year)
                      (setf (display-month widget) month))) name))

(defmethod month-name ((widget calendar-widget))
  (nth (display-month widget) *month-names*))

(defmethod render-widget-body ((widget calendar-widget) &rest args)
  (declare (ignore args))
  (create-month-nav-link "Previous" widget -1)
  (with-html (:br))
  (create-month-nav-link "Next" widget 1)

  (with-html
    (:table
      (:tr (:td :colspan "7" (str (format nil "~a ~a" (month-name widget) (display-year widget)))))
      (:tr (:th "So" ) (:th "Mo") (:th "Di") (:th "Mi") (:th "Do") (:th "Fr") (:th "Sa"))
      (let ((months (build-calendar (display-year widget) (display-month widget)))
            (day-counter 0))
        (loop for month in months do
          (let ((current-year (caar month))
                (current-month (cdar month))
                (days (cdr month)))
              (loop for current-day in days do
                (when (zerop (mod day-counter 7)) (with-html (:tr)))
                (incf day-counter)
                (cl-who:htm (:td :class (when (equal (list current-day current-month current-year)
                                              (selected-date widget))
                                       "current-day")
                      :onclick (parenscript:ps* `(setf (slot-value (document.get-element-by-id "event-date")
                                                                   'value)
                                                       ,(format nil "~D.~D.~D" current-day
                                                                               current-month
                                                                               current-year)))
                  (render-link (let ((current-year current-year)
                                     (current-month current-month)
                                     (current-day current-day))
                                 (f_% (setf (selected-date widget) (list current-day current-month current-year))))
                               (cl-who:escape-string (format nil "~D" current-day))))))))))))

(defmethod dependencies append ((widget calendar-widget))
  (list (make-local-dependency :stylesheet "calendar")))
