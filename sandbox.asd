(defsystem :sandbox
  :depends-on (:alexandria)
  :serial t
  :components ((:file "sandbox-impl")
               (:file "sandbox-pkg")))
