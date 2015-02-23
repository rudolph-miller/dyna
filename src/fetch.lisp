(in-package :cl-user)
(defpackage dyna.fetch
  (:use :cl
        :dyna.request)
  (:import-from :drakma
                :http-request))
(in-package :dyna.fetch)

(setf r (make-request :method "post"
                      :service "dynamodb"
                      :region "ap-northeast-1"
                      :endpoint "dynamodb.ap-northeast-1.amazonaws.com"
                      :credentials `(,(asdf::getenv "AWS_ACCESS_KEY") . ,(asdf::getenv "AWS_SECRET_KEY"))
                      :signed-headers `(("host" . ,#'request-host)
                                        ("content-type" . ,#'request-content-type)
                                        ("x-amz-date" . ,#'request-x-amz-date)
                                        ("x-amz-target" . ,(concatenate 'string "DynamoDB_20120810." "ListTables")))
                      :content-type "application/x-amz-json-1.0"))

(defun fetch (request operation content)
  (let ((url (concatenate 'string "http://" (request-endpoint request))))
    (setf (request-content request) content)
    (http-request url
                  :method :post
                  :additional-headers `(("Authorization" . ,(request-authorization request))
                                        ("X-Amz-Date" . ,(request-x-amz-date request))
                                        ("X-Amz-Target" . ,(concatenate 'string "DynamoDB_20120810." operation)))
                  :content-type (request-content-type request)
                  :content content)))
