(in-package :cl-user)
(defpackage dyna.fetch
  (:use :cl
        :dyna.request))
(in-package :dyna.fetch)

(syntax:use-syntax :annot)

@export
(defvar *local-port* 8000)

@export
(defun fetch (credentials region operation content)
  (let* ((target (concatenate 'string "DynamoDB_20120810." operation))
         (endpoint (if (string= region "local")
                       (format nil "localhost:~d" *local-port*)
                       (format nil "dynamodb.~a.amazonaws.com" region)))
         (request (make-request :service "dynamodb"
                                :method "post"
                                :content content
                                :region region
                                :endpoint endpoint
                                :content-type "application/x-amz-json-1.0"
                                :credentials credentials
                                :signed-headers `(("host" . ,#'request-host)
                                                  ("content-type" . ,#'request-content-type)
                                                  ("x-amz-date" . ,#'request-x-amz-date)
                                                  ("x-amz-target" . ,target))))
         (url (concatenate 'string "http://" (request-endpoint request))))
    (handler-bind ((dex:http-request-failed (lambda (c)
                                              (declare (ignore c))
                                              (invoke-restart 'dex:ignore-and-continue))))
      (dex:post url :headers `((:host . ,(request-host request))
                               (:authorization . ,(request-authorization request))
                               (:x-amz-date . ,(request-x-amz-date request))
                               (:x-amz-target . ,target)
                               (:content-type . ,(request-content-type request)))
                    :content content))))
