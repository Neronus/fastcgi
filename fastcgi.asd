;;;; fastcgi.asd

(asdf:defsystem #:fastcgi
  :serial nil
  :depends-on (#:cffi
               #:trivial-gray-streams)
  :components ((:file "package")
               (:file "low-level" :depends-on ("package"))
               (:file "high-level" :depends-on ("package" "low-level")))

  :author "Christian von Essen <christian@mvonessen.de>"
  :license "FreeBSD (see LICENSE)"
  :description "Two-layered binding to libfcgi using CFFI.

The low-layer binding provides direct access to libfcgi. The docstrings
are taken directly from its header. If you want to know how to use
it, consult any documentation about how to use libfcgi.

The high-level binding mostly differs from the low-level binding
in that it provides easier access to the environment variables
\(via MAP-ENVIRONMENT, GET-ENVIRONMENT and MAP-ENVIRONMENT-ACONS)
and by wrapping the native streams in gray-streams so that
you can use them like any other lisp stream.")

