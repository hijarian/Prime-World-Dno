;;;; runner.lisp
;;; A way to run the webapp locally, in the localhost:4242 URL.

(in-package :cl-user)

#+sbcl (require :sb-posix)

;; You should have project (or a symlink to it) in your `quicklisp/local-projects` directory
(ql:quickload :dno)

;; Now you can call (dno:run) and your server will run
(dno:run)
