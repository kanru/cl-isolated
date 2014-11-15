(asdf:defsystem :sandbox
  :version "1.0"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "AGPLv3+"
  :description "A restricted environment for Common Lisp code evaluation"
  :depends-on (:alexandria)
  :components
  ((:file "sandbox-impl")
   (:file "sandbox-cl" :depends-on ("sandbox-impl"))
   (:file "sandbox" :depends-on ("sandbox-impl"))))
