(asdf:defsystem :isolated
  :version "1.1"
  :author "Kan-Ru Chen <kanru@kanru.info>"
  :licence "AGPLv3+"
  :description "A isolated environment for Common Lisp code evaluation"
  :depends-on (:alexandria)
  :components
  ((:file "isolated-impl")
   (:file "isolated-cl" :depends-on ("isolated-impl"))
   (:file "isolated" :depends-on ("isolated-impl"))))
