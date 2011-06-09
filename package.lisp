(defpackage :gplt (:use :cl)
  (:export :*gnuplot-process*
	   :*gnuplot-input*
	   :*gnuplot-output*
	   :gpstart
	   :gpstop
	   :gprestart
	   :gpdisplay
	   :gpexec
	   :make-plot))
