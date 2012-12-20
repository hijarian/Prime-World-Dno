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
                 #:cl-json
                 #:hunchentoot
                 #:parenscript
                 #:cl-who
                 #:css-lite)
    :components ((:file "package")
                 (:module :src
                          :serial t
                          :components ((:file "helpers")
                                       (:file "dno")
                                       (:file "init")))))


