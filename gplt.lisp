(defpackage :gplt (:use :cl))

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

;; functions to convert lisp clauses into infix form

(defun insert-op (op args)
  (let ((clause (list (first args))))
    (dolist (arg (rest args))
      (push op clause)
      (push arg clause))
    (nreverse clause)))

(defun parse-prefix (clause)
  (cond ((or (numberp clause) (symbolp clause))
	 clause)
	((= (length clause) 2)
	 (case (first clause)
	   ((or sin cos tan) `(,(first clause) (,(parse-prefix (second clause)))))
	   (+ (parse-prefix (second clause)))
	   (* (parse-prefix (second clause)))
	   (- `(- ,(parse-prefix (second clause))))
	   (/ `(1 / ,(parse-prefix (second clause))))
	   (exp `(exp (,(parse-prefix (second clause)))))))
	((and (= (length clause) 3)
	      (not (member (first clause) '(+ - * /))))
	 (case (first clause)
	   (expt `(,(parse-prefix (second clause)) ** ,(parse-prefix (third clause))))))
	(t (insert-op (first clause)
		      (mapcar #'parse-prefix (rest clause))))))
      
(defun infix-to-string (infix)
  (string-downcase (format nil "~A" infix)))

;; gnuplot ops

(defun set-param (fd param val)
  (format fd "set ~A ~A~%" (string-downcase (string param)) val))

(defun unset-param (fd param)
  (format fd "unset ~A~%" (string-downcase (string param))))

;; simple gnuplot call
#|
(defun gplt (&key func file (params "with lines") (sets '((term "x11"))) (unsets '(key)))
  (let ((fd (open-pipe)))
    (progn
      (dolist (k sets)
	 (set-param fd (first k) (second k)))
      (dolist (k unsets)
	(unset-param fd k))
      (cond ((and (null func) (null file))
	     (error "function or data file needed"))
	    ((and (not (null func)) (not (null file)))
	     (error "don't set both function and data"))
	    ((not (null file))
	     (let ((dim 0))
	       (with-open-file (s file)
		 (let ((str (make-string-input-stream (read-line s))))
		   (loop (if (null (read str)) (return) (incf dim)))))
	       (format fd (cond ((< dim 3) "plot ~A ~A~%" file params)
				((= dim 3) "splot ~A ~A~%" file params)
				(t (error "can't plot dimensions more than 3, check data file"))))))
	    ((not (null func))
	     (let ((func-str (infix-to-string (parse-prefix func))))
	       (format fd (if (find #\y func-str) "splot ~A ~A~%" "plot ~A ~A~%") func-str params))))
       (close fd))))
|#

;; interactive mode

(defvar *gnuplot-input*)
(defvar *gnuplot-output*)
(defvar *gnuplot-process*)

(defun gplt-start ()
  (multiple-value-setq (*gnuplot-input* *gnuplot-output* *gnuplot-process*)
    (open-pipe))
  (when (sb-ext:process-alive-p *gnuplot-process*) T))

(defun convert-clause (clause)
  (case (first clause)
    (range (format nil " [~A:~A] " (second clause) (third clause)))
    (enum (format nil " ~A,~A " (second clause) (third clause)))))

(defun parse-cmd (cmd)
  (mapcar #'(lambda (c) (if (listp c) (convert-clause c) c)) cmd))

(defun gplt-exec (cmd)
  (let ((clause
	 (case (first cmd)
	   ((or plot splot)
	      (append (list (first cmd)
			    (if (stringp (second cmd))
				(second cmd)
				(parse-prefix (second cmd))))
		      (parse-cmd (cddr cmd))))
	   (t (parse-cmd cmd)))))
    (format *gnuplot-input* "~A"
	    (string-trim "()" (string-downcase (format nil "~A~%" clause))))
    ))
;    (format *query-io* "~A~%" (read-line *gnuplot-output*))))
      
(defun gplt-display ()
  (close *gnuplot-input*)
  (close *gnuplot-output*))

(defun gplt-stop ()
  (when (sb-ext:process-alive-p *gnuplot-process*)
    (sb-ext:process-kill *gnuplot-process* 9)))

(defun gplt-restart ()
  (gplt-stop)
  (gplt-start))

(defun sharp-G (stream &rest ignore)
  (declare (ignore ignore))
  (gplt-exec (read stream t nil t)))

(set-dispatch-macro-character #\# #\G #'sharp-G)
