;;;; package.lisp

(defpackage #:fastcgi.low-level
  (:use #:cl)
  (:export
   #:request
   #:request-out
   #:request-in
   #:request-err
   #:request-role
   #:request-id
   #:request-envp
   #:with-request
   #:get-error 
   #:accept-r 
   #:flush 
   #:open-socket 
   #:get-str 
   #:put-str 
   #:stream-close 
   #:get-line 
   #:start-filter-data 
   #:get-param 
   #:close-socket 
   #:is-cgi 
   #:init 
   #:put-s 
   #:free 
   #:finish-r 
   #:init-request 
   #:unget-char 
   #:fcgx-finish 
   #:get-char 
   #:accept 
   #:has-seen-eof 
   #:set-exit-status 
   #:put-char 
   #:clear-error))

(defpackage #:fastcgi.high-level
  (:use #:cl #:fastcgi.low-level #:trivial-gray-streams)
  (:export
   #:request
   #:request-get-out
   #:request-get-in
   #:request-get-err
   #:request-role
   #:request-id
   #:request-envp
   #:with-request
   #:get-error 
   #:accept-r 
   #:flush 
   #:open-socket 
   #:start-filter-data 
   #:get-param 
   #:close-socket 
   #:is-cgi 
   #:init 
   #:free 
   #:finish-r 
   #:init-request 
   #:unget-char 
   #:fcgx-finish 
   #:get-char 
   #:accept 
   #:has-seen-eof 
   #:set-exit-status 
   #:put-char 
   #:clear-error

   #:map-environment
   #:map-environment-acons
   #:get-environment
   #:environment-find)
  (:shadow #:set-exit-status))

