(in-package :cl-user)
(defpackage dyna.operation
  (:use :cl
        :jsown
        :dyna.error
        :dyna.util
        :dyna.structure
        :dyna.fetch
        :dyna.content)
  (:import-from :flexi-streams
                :octets-to-string))
(in-package :dyna.operation)

(syntax:use-syntax :annot)

(defvar *result* nil)

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
                  (t (error '<dyna-request-error> :status status
                                                  :message (octets-to-string result)
                                                  :meta meta))))))))

(defoperation batch-get-item
  (values (let ((result))
            (do-json-keys (key val)
                          (val *result* "Responses")
              (push (cons key
                          (mapcar #'(lambda (item)
                                      (parse-result-item item))
                                  val))
                    result))
            result)
          *result*))

(defoperation batch-write-item
  (values t *result*))

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
