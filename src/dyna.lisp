(in-package :cl-user)
(defpackage dyna
  (:use :cl
        :jsown
        :annot.class
        :dyna.error
        :dyna.util
        :dyna.content
        :dyna.request
        :dyna.fetch)
  (:import-from :flexi-streams
                :octets-to-string)
  (:export :make-dyna
           :dyna-credentials
           :dyna-region
           :batch-get-item
           :batch-write-item
           :create-table
           :delete-item
           :delete-table
           :describe-table
           :get-item
           :list-tables
           :put-item
           :query
           :scan
           :update-item
           :upadate-table))
(in-package :dyna)

(syntax:use-syntax :annot)

(defvar *result* nil)

(defstruct dyna
  (credentials)
  (region "us-east-1"))

(defmacro defoperation (operation &body body)
  `(defun ,operation (dyna &rest args)
     (let ((op (operation->opration-for-fetch ',operation))
           (content (apply (intern (format nil "~a-CONTENT" ',operation)
                                   (find-package :dyna.content))
                           dyna
                           args)))
       (multiple-value-bind (result status meta) (fetch (dyna-credentials dyna)
                                                        (dyna-region dyna)
                                                        op
                                                        content)
         (cond
           ((= status 200)
            (let ((*result* (parse (octets-to-string result))))
              ,@body))
           (t (error '<dyna-request-error> :message (octets-to-string result) :meta meta)))))))

(defoperation batch-get-item)

(defoperation batch-write-item)

(defoperation create-table
  (values t *result*))

(defoperation delete-item
  (values t *result*))

(defoperation delete-table
  (values t *result*))

(defoperation describe-table
  *result*)

(defoperation get-item
  (values (parse-result-item (val *result* "Item")) *result*))

(defoperation list-tables
  (values (val *result* "TableNames") *result*))

(defoperation put-item
  (values t *result*))

(defoperation query)

(defoperation scan)

(defoperation update-item
  (values t *result*))

(defoperation update-table
  (values t *result*))
