(defsystem :eval-bot
  :depends-on (:bordeaux-threads :trivial-irc :alexandria :split-sequence)
  :components
  ((:file "sandbox-impl")
   (:file "sandbox-cl" :depends-on ("sandbox-impl"))
   (:file "common")
   (:file "eval-bot" :depends-on ("common" "sandbox-impl"))))
