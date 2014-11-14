(defsystem :sandbox
  :depends-on (:alexandria)
  :components
  ((:file "sandbox-impl" :depends-on ("common"))
   (:file "sandbox-extra" :depends-on ("common" "sandbox-impl" "sandbox-cl"))
   (:file "sandbox-cl" :depends-on ("sandbox-impl"))
   (:file "common")))
