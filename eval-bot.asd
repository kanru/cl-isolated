(defsystem :eval-bot
  :depends-on (:bordeaux-threads :trivial-irc :alexandria
                                 :split-sequence)
  :serial t
  :components ((:file "sandbox-impl")
               (:file "sandbox-pkg")
               (:file "clhs-url")
               (:file "eval-bot")))
