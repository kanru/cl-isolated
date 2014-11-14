(defsystem :sandbox
  :depends-on (:alexandria)
  :components
  ((:file "sandbox-impl")
   (:file "sandbox-cl" :depends-on ("sandbox-impl"))))
