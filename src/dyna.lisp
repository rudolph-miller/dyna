(in-package :cl-user)
(defpackage dyna
  (:use :cl
        :jsown
        :split-sequence
        :annot.class
        :dyna.error
        :dyna.util
        :dyna.content
        :dyna.request
        :dyna.fetch)
  (:import-from :flexi-streams
                :octets-to-string)
  (:export :make-dyna))
(in-package :dyna)

(syntax:use-syntax :annot)

(defvar *result* nil)

@export
@export-accessors
(defstruct dyna
  (credentials)
  (region "us-east-1"))

(defmacro defoperation (operation &body body)
  `(progn (export ',operation)
          (defun ,operation (dyna &rest args)
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
                  (t (error '<dyna-request-error> :message (octets-to-string result) :meta meta))))))))

(defun operation->opration-for-fetch (op)
  (format nil "~{~:(~a~)~}" (split-sequence #\- (symbol-name op))))

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

(defoperation update-item)

(defoperation update-table
  (values t *result*))
