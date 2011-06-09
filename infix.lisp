(in-package :gplt)

;; functions to convert lisp clauses into infix form

(defun insert-op (op args)
  (let ((clause (list (first args))))
    (dolist (arg (rest args))
      (push op clause)
      (push arg clause))
    (nreverse clause)))

(defun parse-prefix (clause)
  (cond ((stringp clause) clause)
	((eq clause 'loop) "'-'")
	((or (numberp clause) (symbolp clause)) clause)
	((= (length clause) 2)
	 (case (first clause)
	   ((sin cos tan asin acos atan sinh cosh tanh exp log abs)
	      `(,(first clause) (,(parse-prefix (second clause)))))
	   ((+ *) (parse-prefix (second clause)))
	   (- `(- ,(parse-prefix (second clause))))
	   (/ `(1 / ,(parse-prefix (second clause))))
	   (error "Cannot convert ~A to infix" clause)))
	((and (= (length clause) 3) (eq (first clause) 'expt))
	 `(,(parse-prefix (second clause)) ** ,(parse-prefix (third clause))))
	(t (insert-op (first clause)
		      (mapcar #'parse-prefix (rest clause))))))

(defun infix-to-string (infix)
  (string-downcase (format nil "~A" infix)))

;; convert some special gnuplot expressions

(defun convert-clause (clause)
  (case (first clause)
    (range (format nil " [~A:~A] " (second clause) (third clause)))
    (enum (apply #'format nil (apply #'concatenate 'string
				     (apply #'concatenate 'string
					    (append '(" ") (make-list (- (length clause) 2) :initial-element "~A,") '("~A ")))
		 (cdr clause))))
    (t clause)))

(defun parse-cmd (cmd)
  (mapcar #'(lambda (c) (if (listp c) (convert-clause c) c)) cmd))
