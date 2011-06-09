(defpackage :gplt-system
    (:use :cl :asdf))

(in-package :gplt-system)

(defsystem gplt
  :name "gplt"
  :description "Gnuplot interface"
  :author "Khokhlov Ivan"
  :licence "BSD"
  :depends-on ()
  :serial t
  :components
  ((:file "package")
   (:file "infix")
   (:file "interact")
   (:file "packet")))
