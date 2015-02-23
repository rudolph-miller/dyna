(in-package :cl-user)
(defpackage dyna.fetch
  (:use :cl
        :dyna.request)
  (:import-from :drakma
                :http-request))
(in-package :dyna.fetch)

(syntax:use-syntax :annot)

@export
(defun fetch (credentials region operation content)
    (let* ((target (concatenate 'string "DynamoDB_20120810." operation))
           (request (make-request :service "dynamodb"
                                  :method "post"
                                  :content content
                                  :region region
                                  :endpoint (format nil "dynamodb.~a.amazonaws.com" region)
                                  :content-type "application/x-amz-json-1.0"
                                  :credentials credentials
                                  :signed-headers `(("host" . ,#'request-host)
                                                    ("content-type" . ,#'request-content-type)
                                                    ("x-amz-date" . ,#'request-x-amz-date)
                                                    ("x-amz-target" . ,target))))
           (url (concatenate 'string "http://" (request-endpoint request))))
      (http-request url
                    :method :post
                    :additional-headers `(("Authorization" . ,(request-authorization request))
                                          ("X-Amz-Date" . ,(request-x-amz-date request))
                                          ("X-Amz-Target" . ,target))
                    :content-type (request-content-type request)
                    :content content)))
