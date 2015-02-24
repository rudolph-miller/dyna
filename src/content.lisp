(in-package :cl-user)
(defpackage dyna.content
  (:use :cl
        :jsown
        :dyna.util
        :dyna.error)
  (:import-from :alexandria
                :plist-alist
                :make-keyword))
(in-package :dyna.content)

(syntax:use-syntax :annot)

(defun check-incomplete (args must)
  (loop with result
        for arg in must
        unless (getf args (make-keyword arg))
          do (push arg result)
        finally (unless (null result)
                  (error '<dyna-incomplete-argumet-error> :args result))))

(defmacro defcontent (name lst must &body body)
  (let ((content-symbol (intern (format nil "~a-CONTENT" name))))
    `(progn (export ',content-symbol)
            (defun ,content-symbol (dyna &rest args ,@lst)
              (declare (ignorable dyna))
              (check-incomplete args ',must)
              (let ((result (progn ,@body)))
                (if (null result)
                    "{}"
                    (to-json (add-obj-to-list result))))))))

(defun build-secondary-index-obj (lst)
  (mapcar #'(lambda (index)
              (add-obj-to-list
               (mapcar #'(lambda (item)
                           (cond ((string= (car item) "Projection")
                                  (cons (car item) (add-obj-to-list (cdr item))))
                                 ((string= (car item) "KeySchema")
                                  (cons (car item) (mapcar #'(lambda (item)
                                                               (add-obj-to-list item))
                                                           (cdr item))))
                                 (t item)))
                       index)))
          lst))

(defcontent batch-get-item () ())

(defcontent batch-write-item () ())

(defcontent create-table (&key table-name attribute-definitions key-schema global-secondary-indexes
                               local-secondary-indexes provisioned-throughput)
    (table-name attribute-definitions key-schema provisioned-throughput)
  (append (list  `("TableName" . ,table-name)
                 `("AttributeDefinitions" . ,(build-obj-list attribute-definitions))
                 `("KeySchema" . ,(build-obj-list key-schema))
                 `("ProvisionedThroughput" . ,(add-obj-to-list provisioned-throughput)))
          (when local-secondary-indexes
            (list `("LocalSecondaryIndexes" . ,(build-secondary-index-obj local-secondary-indexes))))
          (when global-secondary-indexes
            (list `("GlobalSecodaryIndexes" . ,(build-secondary-index-obj global-secondary-indexes))))))


(defcontent delete-item (&key table-name key condition-expression return-values)
    (table-name key)
  (append (list  `("TableName" . ,table-name)
                 `("Key" . ,(add-obj-to-list (build-desc-list key))))
          (when return-values
            (list `("ReturnValues" . ,return-values)))
          (when condition-expression
            (list `("ConditionExpression" . ,condition-expression)))))

(defcontent delete-table (&key table-name)
    (table-name)
  `(("TableName" . ,table-name)))

(defcontent describe-table (&key table-name)
    (table-name)
  `(("TableName" . ,table-name)))

(defcontent get-item (&key table-name key projection-expression consistent-read return-consumed-capacity)
    (table-name key)
  (append (list  `("TableName" . ,table-name)
                 `("Key" . ,(add-obj-to-list (build-desc-list key))))
          (when projection-expression
            (list `("ProjectionExpression" . ,projection-expression)))
          (when consistent-read
            (list `("ConsistentRead" . ,consistent-read)))
          (when return-consumed-capacity
            (list `("ReturnConsumedCapacity" . ,return-consumed-capacity)))))

(defcontent list-tables () nil)

(defcontent put-item (&key table-name item condition-expression expression-attribute-values)
    (table-name item)
  (append (list  `("TableName" . ,table-name)
                 `("Item" . ,(add-obj-to-list (build-desc-list item))))
          (when condition-expression
            (list `("ConditionExpression" . ,condition-expression)))
          (when expression-attribute-values
            (list `("ExpressionAttributeValues" . ,(add-obj-to-list (build-desc-list expression-attribute-values)))))))

(defcontent query () ())

(defcontent scan () ())

(defcontent update-item () ())

(defcontent update-table (&key table-name attribute-definitions provisioned-throughput)
    (table-name)
  (append (list  `("TableName" . ,table-name))
          (when attribute-definitions
            (list `("AttributeDefinitions" . ,(build-obj-list attribute-definitions))))
          (when provisioned-throughput
            (list `("ProvisionedThroughput" . ,(add-obj-to-list provisioned-throughput))))))
