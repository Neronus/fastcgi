(in-package #:fastcgi.low-level)

(cffi:define-foreign-library fcgi
    (t (:default "libfcgi")))

(cffi:use-foreign-library fcgi)

(defparameter *FCGX_UNSUPPORTED_VERSION* -2)
(defparameter *FCGX-PROTOCOL-ERROR* -3)
(defparameter *FCGX-PARAMS-ERROR* -4)
(defparameter *FCGX-CALL-SEQ-ERROR* -5)

(cffi:defctype param-array (:pointer :string))
(cffi:defctype fcgi-stream  :pointer)

(defparameter *FCGI-FAIL-ACCEPT-ON-INTR*	1)

(cffi:defcstruct request
  (request-id         :int)
  (role               :int)
  (in                 fcgi-stream)
  (out                fcgi-stream)
  (err                fcgi-stream)
  (envp               param-array)
  ;; Don't use anything below here
  (params             :pointer)
  (ipc-fd             :pointer)
  (is-begin-processed :int)
  (keep-connection    :int)
  (appstatus          :int)
  (n-writer           :int)
  (flags              :int)
  (listen-sock        :int))

#+nil (declaim (ftype (function (request) fcgi-stream)
                request-out
                request-in
                request-err))
(defun request-out (request)
  "Get the OUT stream of REQUEST."
  (cffi:foreign-slot-value request 'request 'out))

(defun request-in (request)
  "Get the IN stream of REQUEST."
  (cffi:foreign-slot-value request 'request 'in))

(defun request-err (request)
  "Get the ERR stream of REQUEST."
  (cffi:foreign-slot-value request 'request 'err))

(defun request-id (request)
  "Get the request id of REQUEST"
  (cffi:foreign-slot-value request 'request 'request-id))

(defun request-role (request)
  "Get the role of REQUEST"
  (cffi:foreign-slot-value request 'request 'role))

#+nil (declaim (ftype (function (request) param-array) request-envp))
(defun request-envp (request)
  "Get the environment array of REQUEST"
  (cffi:foreign-slot-value request 'request 'envp))

(defmacro with-request ((name socket &optional close) &body body)
  "Binds an initialized request structure to the given name.

Socket is the socket the request should listen to.
If close is not nil, then the socket will be closed
at the end of the body."
  `(cffi:with-foreign-object (,name 'request)
     (init-request ,name ,socket 0)
     (unwind-protect
          (progn
            ,@body)
       (free ,name (if ,close 1 0)))))

(cffi:defcfun ("FCGX_IsCGI" is-cgi) :int
    "Returns TRUE iff this process appears to be a CGI process rather
    than a FastCGI process.")

(cffi:defcfun ("FCGX_Init" init) :int
    "Initialize the FCGX library.  Call in multi-threaded apps
     before calling FCGX_Accept_r().

     Returns 0 upon success.")

(cffi:defcfun ("FCGX_OpenSocket" open-socket) :int
 "Create a FastCGI listen socket.

 path is the Unix domain socket (named pipe for WinNT), or a colon
 followed by a port number.  e.g. \"/tmp/fastcgi/mysocket\", \":5000\"

 backlog is the listen queue depth used in the listen() call.

 Returns the socket's file descriptor or -1 on error."
 (path :string) (backlog :int))

(cffi:defcfun ("close" close-socket) :int
  "Close a socket opened with OPEN-SOCKET."
  (fd :int))

(cffi:defcfun ("FCGX_InitRequest" init-request) :int
  "Initialize a FCGX_Request for use with FCGX_Accept_r().

sock is a file descriptor returned by FCGX_OpenSocket() or 0 (default).
The only supported flag at this time is FCGI_FAIL_ON_INTR.

Returns 0 upon success."
    (request :pointer) (sock :int) (flags :int))

(cffi:defcfun ("FCGX_Accept_r" accept-r) :int
  "a new request (multi-thread safe).  Be sure to call
	INIT first.

Results:
 0 for successful call, -1 for error.

Side effects:

     Finishes the request accepted by (and frees any
     storage allocated by) the previous call to FCGX_Accept.
     Creates input, output, and error streams and
     assigns them to *in, *out, and *err respectively.
     Creates a parameters data structure to be accessed
     via getenv(3) (if assigned to environ) or by FCGX_GetParam
     and assigns it to *envp.

     DO NOT retain pointers to the envp array or any strings
     contained in it (e.g. to the result of calling FCGX_GetParam),
     since these will be freed by the next call to FCGX_Finish
     or FCGX_Accept.

 DON'T use the FCGX_Request, its structure WILL change.
"
  (request :pointer))

(cffi:defcfun ("FCGX_Finish_r" finish-r) :void
"Finish the request (multi-thread safe).

Side effects:
     Finishes the request accepted by (and frees any
     storage allocated by) the previous call to FCGX_Accept.

     DO NOT retain pointers to the envp array or any strings
     contained in it (e.g. to the result of calling FCGX_GetParam),
     since these will be freed by the next call to FCGX_Finish
     or FCGX_Accept."
  (request :pointer))

(cffi:defcfun ("FCGX_Free" free) :void
"     Free the memory and, if close is true, 
     IPC FD associated with the request (multi-thread safe)."
(request :pointer) (close :int))

(cffi:defcfun  ("FCGX_Accept" accept) :int
    "Accept a new request (NOT multi-thread safe).

Results:
 0 for successful call, -1 for error.

Side effects:

     Finishes the request accepted by (and frees any
     storage allocated by) the previous call to FCGX_Accept.
     Creates input, output, and error streams and
     assigns them to *in, *out, and *err respectively.
     Creates a parameters data structure to be accessed
     via getenv(3) (if assigned to environ) or by FCGX_GetParam
     and assigns it to *envp.

     DO NOT retain pointers to the envp array or any strings
     contained in it (e.g. to the result of calling FCGX_GetParam),
     since these will be freed by the next call to FCGX_Finish
     or FCGX_Accept."
  (in fcgi-stream)
  (out fcgi-stream)
  (err fcgi-stream)
  (envp fcgi-stream))

(cffi:defcfun "FCGX_Finish" :void
  "Finish the current request (NOT multi-thread safe).

Side effects:
     Finishes the request accepted by (and frees any
     storage allocated by) the previous call to FCGX_Accept.

     DO NOT retain pointers to the envp array or any strings
     contained in it (e.g. to the result of calling FCGX_GetParam),
     since these will be freed by the next call to FCGX_Finish
     or FCGX_Accept.
")

(cffi:defcfun ("FCGX_StartFilterData" start-filter-data) :int
    "STREAM is an input stream for a FCGI_FILTER request.
     stream is positioned at EOF on FCGI_STDIN.
     Repositions stream to the start of FCGI_DATA.
     If the preconditions are not met (e.g. FCGI_STDIN has not
     been read to EOF) sets the stream error code to
     FCGX_CALL_SEQ_ERROR.

Results:
     0 for a normal return, < 0 for error"
    (stream fcgi-stream))

(cffi:defcfun ("FCGX_SetExitStatus" set-exit-status) :void
  "Sets the exit status for stream's request. The exit status
     is the status code the request would have exited with, had
     the request been run as a CGI program.  You can call
     SetExitStatus several times during a request; the last call
     before the request ends determines the value."
  (status :int) (stream fcgi-stream))

(cffi:defcfun ("FCGX_GetParam" get-param) :string
  "obtain value of FCGI parameter in environment.

The returned string is not sharing data with anything else.
Here this library diverges from the C library."
  (name :string) (envp :pointer))

(cffi:defcfun ("FCGX_GetChar" get-char) :int
  "Reads a byte from the input stream and returns it.

   Results:
	The byte, or EOF (-1) if the end of input has been reached."
  (stream :pointer))

(cffi:defcfun ("FCGX_UnGetChar" unget-char) :int
  "Reads a byte from the input stream and returns it.

   Results:
	The byte, or EOF (-1) if the end of input has been reached."
  (c :int) (stream :pointer))


(cffi:defcfun ("FCGX_GetStr" get-str) :int
    "Reads up to n consecutive bytes from the input stream
     into the character array str.  Performs no interpretation
     of the input bytes.

Results:
 Number of bytes read.  If result is smaller than n,
     the end of input has been reached."
    (str (:pointer :char)) (n :int) (stream fcgi-stream))

(cffi:defcfun ("FCGX_GetLine" get-line) (:pointer :char)
  "Reads to n-1 consecutive bytes from the input stream have been read
     if '\n' or EOF is read. The terminating '\n' is copied to str.
     After copying the last byte into str, stores a '\0' terminator.

Results:
 NULL if EOF is the first thing read from the input stream,
 str otherwise."
  (str (:pointer :char)) (n :int) (stream fcgi-stream))


(cffi:defcfun ("FCGX_HasSeenEOF" has-seen-eof) :int
  "Returns EOF if end-of-file has been detected while reading
     from stream; otherwise returns 0.

     Note that FCGX_HasSeenEOF(s) may return 0, yet an immediately
     following FCGX_GetChar(s) may return EOF.  This function, like
     the standard C stdio function feof, does not provide the
     ability to peek ahead.

Results:
 EOF if end-of-file has been detected, 0 if not."
    (stream fcgi-stream))


(cffi:defcfun ("FCGX_PutChar" put-char) :int
  "Writes a byte to the output stream.

Results:
 The byte, or EOF (-1) if an error occurred."
  (c :int) (stream fcgi-stream))


(cffi:defcfun ("FCGX_PutStr" put-str) :int
    "Writes n consecutive bytes from the character array str
     into the output stream.  Performs no interpretation
     of the output bytes.

Results:
     Number of bytes written (n) for normal return,
     EOF (-1) if an error occurred."
    (str (:pointer :char)) (n :int) (stream fcgi-stream))

(cffi:defcfun ("FCGX_PutS" put-s) :int
  "Writes a null-terminated character string to the output stream.
Results:
     number of bytes written for normal return,
     EOF (-1) if an error occurred."
  (str :string) (stream fcgi-stream))



(cffi:defcfun ("FCGX_FFlush" flush) :int
    "Flushes any buffered output.


     Server-push is a legitimate application of FCGX_FFlush.
     Otherwise, FCGX_FFlush is not very useful, since FCGX_Accept
     does it implicitly.  Calling FCGX_FFlush in non-push applications
     results in extra writes and therefore reduces performance.

Results:
     EOF (-1) if an error occurred."
    (stream fcgi-stream))


(cffi:defcfun ("FCGX_FClose" stream-close) :int
  "Closes the stream. For writers, flushes any buffered output.

      Close is not a very useful operation since FCGX_Accept
      does it implicitly.  Closing the out stream before the
      err stream results in an extra write if there's nothing
      in the err stream, and therefore reduces performance.

Results:
     EOF (-1) if an error occurred."
  (stream fcgi-stream))

(cffi:defcfun ("FCGX_GetError" get-error) :int
  "Return the stream error code.  0 means no error, > 0
   is an errno(2) error, < 0 is an FastCGI error."
  (stream fcgi-stream))

(cffi:defcfun ("FCGX_ClearError" clear-error) :void
  "Clear the stream error code and end-of-file indication."
  (stream fcgi-stream))
