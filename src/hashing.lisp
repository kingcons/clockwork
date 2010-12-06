(in-package :clockwork)

;; Thanks to Will Halliburton for make-salt and random-string.

(defparameter *the-random-state* (make-random-state t)
  "A fresh random state.")

(defun make-salt ()
  (random-string 10 36))

(defun random-string (&optional (n 10) (base 16))
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~VR" base (random base *the-random-state*)))))

(defun hash-concat (string-list)
  (let ((result (digest-sequence :sha1 (ascii-string-to-byte-array (pop string-list)))))
    (loop for string in string-list do
      (let ((next (format nil "~A~A" (byte-array-to-hex-string result) string)))
	(setf result (digest-sequence :sha1 (ascii-string-to-byte-array next)))))
    (byte-array-to-hex-string result)))
