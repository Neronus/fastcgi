;;;; package.lisp

(defpackage #:fastcgi.low-level
  (:use #:cl)
  (:export
   ;; CGI application functions
   #:is-cgi 
   #:init
   #:finish 
   #:accept
   #:free
   #:start-filter-data
   ;; Working with requests
   #:with-request
   #:request
   #:request-out
   #:request-in
   #:request-err
   #:request-role
   #:request-id
   #:request-envp
   #:init-request 
   #:finish-r 
   #:accept-r
   #:get-param
   ;; Working with sockets
   #:open-socket 
   #:close-socket 
   ;; Working with streams
   #:get-error 
   #:flush 
   #:get-str 
   #:put-str 
   #:stream-close 
   #:get-line 
   #:put-s 
   #:unget-char 
   #:get-char 
   #:has-seen-eof 
   #:set-exit-status 
   #:put-char 
   #:clear-error))

(defpackage #:fastcgi.high-level
  (:use #:cl #:fastcgi.low-level #:trivial-gray-streams)
  (:export
   ;; CGI application functions
   #:is-cgi 
   #:init 
   #:free 
   #:finish 
   #:accept 
   #:start-filter-data 
   ;; Working with requests
   #:with-request
   #:request
   #:request-get-out
   #:request-get-in
   #:request-get-err
   #:request-role
   #:request-id
   #:request-envp
   #:init-request 
   #:finish-r 
   #:accept-r 
   #:get-param
   #:map-environment
   #:map-environment-acons
   #:get-environment
   #:environment-find
   ;; Working with sockets
   #:open-socket 
   #:close-socket
   ;; Working wit streams
   #:get-error 
   #:flush 
   #:unget-char 
   #:get-char 
   #:has-seen-eof 
   #:set-exit-status 
   #:put-char 
   #:clear-error)
  (:shadow #:set-exit-status))

