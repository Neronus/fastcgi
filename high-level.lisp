(in-package fastcgi.high-level)

;;; Gray stream wrapping of request streams
(defclass fcgi-stream (fundamental-stream)
  ((stream :initarg :stream :reader stream-of)))

(defmethod close ((stream fcgi-stream) &key abort)
  (declare (ignore abort))
  (close-socket (stream-of stream))
  nil)

(defclass fcgi-input-stream (fcgi-stream fundamental-binary-input-stream fundamental-character-input-stream)
  ())

(defmethod stream-read-byte ((stream fcgi-input-stream))
  (let ((result (get-char (stream-of stream))))
    (when (= -1 result)
      :eof
      result)))

(defmethod stream-read-char ((stream fcgi-input-stream))
  (code-char (stream-read-byte stream)))

(defmethod stream-unread-char ((stream fcgi-input-stream) char)
  (unget-char (char-code char) (stream-of stream)))

(defclass fcgi-input-stream (fcgi-stream fundamental-binary-input-stream fundamental-character-input-stream)
  ())

(defmethod stream-read-byte ((stream fcgi-input-stream))
  (let ((result (get-char (stream-of stream))))
    (if (= -1 result)
      :eof
      result)))

(defmethod stream-read-char ((stream fcgi-input-stream))
  (let ((char (stream-read-byte stream)))
    (when (eq char :eof)
      :eof
      (code-char char))))

(defmethod stream-unread-char ((stream fcgi-input-stream) char)
  (unget-char (char-code char) (stream-of stream)))

(defclass fcgi-output-stream (fcgi-stream fundamental-binary-output-stream fundamental-character-output-stream)
  ())

(defmethod stream-write-byte ((stream fcgi-output-stream) integer)
  (put-char integer (stream-of stream)))

(defmethod stream-write-char ((stream fcgi-output-stream) char)
  (stream-write-byte stream (char-code char)))

(defmethod stream-terpri ((stream fcgi-output-stream))
  (stream-write-char stream #\Return)
  (stream-write-char stream #\Newline))

(defun map-environment (function request)
  (loop :for i :from 0
        :with env = (request-envp request)
        :for ptr = (cffi:mem-aref env :pointer i)
        :until (cffi:null-pointer-p ptr)
        :do (funcall function (cffi:foreign-string-to-lisp ptr)))
  nil)

(defun map-environment-acons (function request)
  (map-environment
   (lambda (string)
     (let ((pos (position #\= string :test #'char=)))
       (assert pos)
       (funcall function
                (intern (nsubstitute #\- #\_ (string-upcase (subseq string 0 pos))) :keyword)
                (subseq string (+ pos 1)))))
   request)
  nil)

(defun get-environment (request)
  (let ((result nil))
    (map-environment-acons
     (lambda (key value)
       (push (cons key value) result))
     request)
    result))

(defun environment-find (key request)
  (map-environment-acons
   (lambda (key2 value)
     (when (eq key key2)
       (return-from environment-find value)))
   request))

(defun request-get-out (request)
  (make-instance 'fcgi-output-stream
                 :stream (request-out request)))

(defun request-get-err (request)
  (make-instance 'fcgi-output-stream
                 :stream (request-err request)))

(defun request-get-in (request)
  (make-instance 'fcgi-input-stream
                 :stream (request-in request)))

(defun set-exit-status (stream n)
  (declare (type fixnum n))
  (assert (typep stream 'fcgi-stream))
  (fastcgi.low-level:set-exit-status n (stream-of stream)))

