(in-package :cl-user)
(defpackage dyna.request
  (:use :cl
        :annot.class
        :dyna.util)
  (:import-from :ironclad
                :digest-sequence
                :byte-array-to-hex-string)
  (:export :make-request))
(in-package :dyna.request)

(syntax:use-syntax :annot)


@export
@export-accessors
(defstruct request
  (method "POST")
  (credentials)
  (region "us-east-1")
  (service)
  (endpoint)
  (content-type "application/x-amz-json-1.0")
  (signed-headers `((content-type . ,#'request-content-type)
                    (host . ,#'request-host)
                    (x-amz-date . ,#'request-x-amz-date)))
  (canonical-uri "/")
  (query-string "")
  (content ""))

@export
(defun request-access-key (request)
  (car (request-credentials request)))

@export
(defun request-secret-key (request)
  (cdr (request-credentials request)))

@export
(defun request-host (request)
  (request-endpoint request))

@export
(defun request-x-amz-date (request)
  (declare (ignore request))
  (timestamp))

@export
(defun request-content-length (request)
  (etypecase (request-content request)
    ((or vector string) (length (request-content request)))))

@export
(defun %request-signed-headers (request)
  (format nil "~(~{~a~^;~}~)" (mapcar #'car (request-signed-headers request))))

@export
(defun request-canonical-header (request)
  (format nil "~{~(~a~):~a~%~}"
          (mapcan #'(lambda (pair)
                      (list (car pair)
                            (etypecase (cdr pair)
                              ((or symbol function) (funcall (cdr pair) request))
                              (string (cdr pair)))))
                  (alist-sort (request-signed-headers request)))))

@export
(defun request-hashed-payload (request)
  (digest-sha256 (request-content request)))

@export
(defun request-canonical-request (request)
  (format nil "~{~a~^~%~}"
          (list (string-upcase (request-method request))
                (request-canonical-uri request)
                (request-query-string request)
                (request-canonical-header request)
                (%request-signed-headers request)
                (request-hashed-payload request))))

@export
(defun request-hashed-canonical-request (request)
  (digest-sha256 (request-canonical-request request)))


(defun request-hash-algorithm (request)
  (declare (ignore request))
  "AWS4-HMAC-SHA256")

@export
(defun request-credential-scope (request)
  (format nil "~{~a~^/~}"
          (list (datestamp)
                (request-region request)
                (request-service request)
                "aws4_request")))

@export
(defun request-signing-string (request)
  (format nil "~{~a~^~%~}"
          (list (request-hash-algorithm request)
                (timestamp)
                (request-credential-scope request)
                (request-hashed-canonical-request request))))

@export
(defun request-signing-key (request)
  (let* ((k-date (hmac (concatenate 'string "AWS4" (request-secret-key request)) (datestamp)))
         (k-region (hmac k-date (request-region request)))
         (k-service (hmac k-region (request-service request))))
    (hmac k-service "aws4_request")))

@export
(defun request-signature (request)
  (byte-array-to-hex-string
   (hmac (request-signing-key request) (request-signing-string request))))

@export
(defun request-credential (request)
  (format nil "~a/~a"
          (request-access-key request)
          (request-credential-scope request)))

@export
(defun request-authorization (request)
  (format nil "~a ~{~a=~a~^, ~}"
          "AWS4-HMAC-SHA256"
          (list "Credential" (request-credential request)
                "SignedHeaders" (%request-signed-headers request)
                "Signature" (request-signature request))))
