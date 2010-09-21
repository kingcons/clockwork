
(in-package :clockwork)

;;; Multiple stores may be defined. The last defined store will be the
;;; default.
(defstore *clockwork-store* :prevalence
  (merge-pathnames (make-pathname :directory '(:relative "data"))
		   (asdf-system-directory :clockwork)))

