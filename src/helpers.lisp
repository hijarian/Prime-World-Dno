; helpers.lisp
; Helper functions for use in codebase.
; Only Hunchentoot publishers for now.
(in-package :dno)

(defun publish-directory (uri dirname)
  "Makes files in given <dirname> accessible under the URL prefix <uri>. <dirname> should be in relative directory path format, e. g. \"foo/bar/baz\""
  (push (hunchentoot:create-folder-dispatcher-and-handler uri dirname)
        hunchentoot:*dispatch-table*))

(defun publish-file (uri filename)
  (push (hunchentoot:create-static-file-dispatcher-and-handler uri filename)
        hunchentoot:*dispatch-table*))

(defmacro publish-ajax-endpoint (uri name params &body body)
  "<name> is a name of handler function, <uri> is a relative URI for endpoint, <params> is a list of symbol names of expected params in request>"
  `(hunchentoot:define-easy-handler (,name :uri ,uri) ,params 
     (setf (hunchentoot:content-type*) "application/json")
     ,@body))

