(defpackage :gplt (:use :cl)
  (:export :*gnuplot-process*
	   :*gnuplot-input*
	   :*gnuplot-output*
	   :gplt-start
	   :gplt-stop
	   :gplt-restart
	   :gplt-display
	   :gplt-exec
	   :make-plot))

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
  (when (not (null cmd))
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
      )))
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

;; plotting functions

(defun array-row (array row)
  (let ((row-list nil))
    (dotimes (i (array-dimension array 1))
      (push (aref array row i) row-list))
    (nreverse row-list)))

;; single plot

;; comments at some keys:
;; labels: (xlabel ylabel zlabel), when {x,y,z}label is nil - no label
;; ranges: (xrange yrange zrange)
;; draw-with: lines, points, pm3d, etc
;; other-commands: args for gplt-exec

;; data may be function (s-clause) or array nx2 or nx3
;; if s-clause contains symbol 'y' or array has 3 columns,
;; then use 'splot', else 'plot'

(defun make-plot (data &key title ranges labels draw-with color pointsize pointtype terminal output other-commands)
  (gplt-restart)
  (map nil #'gplt-exec
       (remove nil `(,(when title `(set title ,title))
		      (unset key)
		      ,(when terminal `(set term ,terminal))
		      ,(when output `(set output ,(write-to-string output)))
		      ,(when (first labels) `(set xlabel ,(write-to-string (first labels))))
		      ,(when (second labels) `(set ylabel ,(write-to-string (second labels))))
		      ,(when (third labels) `(set zlabel ,(write-to-string (third labels))))
		      ,(when (first ranges) `(set xrange ,(cons 'range (first ranges))))
		      ,(when (second ranges) `(set yrange ,(cons 'range (second ranges))))
		      ,(when (third ranges) `(set zrange ,(cons 'range (third ranges)))))))
  (map nil #'gplt-exec other-commands)
  (let ((plot-params 
	 (remove nil
		 (append 
		  (when draw-with `(with ,draw-with))
		  (when color `(lc rgb ,(format nil "'~A'" color)))
		  (when pointtype `(pt ,pointtype))
		  (when pointsize `(ps ,pointsize))))))
    (cond ((listp data)
	   (gplt-exec
	    (print (append `(,(if (find #\y (string-downcase (write-to-string data)))
				  'splot 'plot)
			      ,data)
			   plot-params))))
	  ((arrayp data)
	   (gplt-exec
	    (append `(,(ecase (array-dimension data 1) (2 'plot) (3 'splot))
		       "'-'")
		    plot-params))
	   (dotimes (i (array-dimension data 0))
	     (gplt-exec (array-row data i)))
	   (gplt-exec '(e)))))
  (gplt-display))

