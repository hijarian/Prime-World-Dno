; heroku-setup.lisp
; Used to initialize the Heroku CL buildpack
(in-package :cl-user)

(print ">>> Building system....")

(load (merge-pathnames *build-dir* "dno.asd"))

(ql:quickload :dno)

(dno:run)

;;; Redefine / extend heroku-toplevel here if necessary.

(print ">>> Done building system")
