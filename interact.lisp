(in-package :gplt)

;; open pipe to gnuplot
;; returnes pipe descriptor and process id

(defun open-pipe ()
  (let ((proc
	 (sb-ext:run-program "gnuplot" '("-persist")
			     :wait nil
			     :search t
			     :input :stream
			     :output :stream)))
    (values (sb-ext:process-input proc) (sb-ext:process-output proc) proc)))

(defvar *gnuplot-input* nil)
(defvar *gnuplot-output* nil)
(defvar *gnuplot-process* nil)

(defun gpstart (&optional dest)
  (if dest
      (setf *gnuplot-input*
	    (cond ((or (eq dest t) (eq dest nil)) dest)
		  (t (open dest :if-exists :supersede :if-does-not-exist :create))))
      (multiple-value-setq (*gnuplot-input* *gnuplot-output* *gnuplot-process*) (open-pipe))))

(defun gpexec (cmd)
  (when (not (null cmd))
    (let ((clause
	   (if (or (stringp cmd) (symbolp cmd))
	       (string-downcase (string cmd))
	       (case (first cmd)
		 ((plot splot)
		    (append `(,(first cmd) ,(parse-prefix (second cmd)))
			    (parse-cmd (cddr cmd))))
		 (t (parse-cmd cmd))))))
      (format *gnuplot-input* "~A~%" (string-downcase (format nil "~{~A ~}" (if (listp clause) clause (list clause))))))))

(defun gpdisplay ()
  (unless (or (eq *gnuplot-input* t)
	  (eq *gnuplot-input* nil))
    (close *gnuplot-input*)
    (when *gnuplot-output* (close *gnuplot-output*))))

(defun gpstop ()
  (when (and *gnuplot-process* (sb-ext:process-alive-p *gnuplot-process*))
    (sb-ext:process-kill *gnuplot-process* 9)))

(defun gprestart (&optional dest)
  (gpstop)
  (gpstart dest))

(defun sharp-G (stream &rest ignore)
  (declare (ignore ignore))
  (gpexec (read stream t nil t)))

(set-dispatch-macro-character #\# #\G #'sharp-G)
