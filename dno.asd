;;;; dno.asd

(asdf:defsystem #:dno
  :serial t
  :description "Presents ratings of players of Prime World as charts."
  :author "Mark Safronov <hijarian@gmail.com>"
  :license "Public Domain"
  :depends-on (#:drakma
               #:cl-ppcre
               #:cl-libxml2
               #:iterate
               #:cl-json)
  :components ((:file "package")
               (:file "dno")))

