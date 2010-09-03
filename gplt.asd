(defpackage :gplt-system
    (:use :cl :asdf))

(in-package :gplt-system)

(defsystem gplt
  :name "gplt"
  :description "Gnuplot interactive interface"
  :author "Khokhlov Ivan"
  :licence "BSD"
  :depends-on ()
  :components
  ((:file "gplt")))
