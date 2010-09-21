(in-package :clockwork)

(defclass reminder ()
  ((email :accessor reminder-email
	  :initarg :email
	  :type string)
   (title :accessor reminder-title
	  :initarg :title
	  :type string)
   (timestamp :accessor reminder-timestamp
	      :initarg :timestamp
	      :type timestamp)
   (repeat-p :accessor reminder-repeat-p
	     :initarg :repeat-p
	     :type boolean)
   (summary :accessor reminder-summary
	    :initarg :summary
	    :type string)
   (at :accessor reminder-at
       :initarg :at
       :type timestamp)
   (style :accessor reminder-style
	  :initarg :style
	  :type keyword)))
