;;;; ants-bot.asd

(asdf:defsystem #:ants-bot
  :depends-on (#:alexandria #:iterate #:usocket)
  :serial t
  :components ((:file "package")
               (:file "ants-bot")))

