; heroku-setup.lisp
; Used to initialize the Heroku CL buildpack
(in-package :cl-user)

(print ">>> Building system....")

; Notice that *build-dir* is a pathname, not a bare string, so we have to use `merge-pathnames` here.
(load (merge-pathnames *build-dir* "dno.asd"))

(ql:quickload :dno)

; We DO NOT evaluate (dno:run) here, because all it does is start a Hunchentoot instance and Heroku CL buildpack already does this itself.

; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
