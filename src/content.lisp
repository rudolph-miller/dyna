(in-package :cl-user)
(defpackage dyna.content
  (:use :cl
        :jsown
        :dyna.util
        :dyna.desc
        :dyna.error
        :dyna.structure)
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

(defcontent batch-get-item (&key request-items return-consumed-capacity)
    (request-items)
  (append (list `("RequestItems" . ,(add-obj-to-list
                                     (mapcar #'(lambda (table)
                                                 (cons (car table)
                                                       (add-obj-to-list
                                                        (mapcar #'(lambda (props)
                                                                    (cons (car props)
                                                                          (if (string= (car props) "Keys")
                                                                              (build-obj-list
                                                                               (mapcar #'(lambda (item)
                                                                                           (build-desc-list item))
                                                                                       (cdr props)))
                                                                              (cdr props))))
                                                                (cdr table)))))
                                             request-items))))
          (when return-consumed-capacity
            (list `("ReturnConsumedCapacity" . ,return-consumed-capacity)))))

(defcontent batch-write-item (&key request-items return-consumed-capacity)
    (request-items)
  (append (list `("RequestItems" . ,(add-obj-to-list
                                     (mapcar #'(lambda (table)
                                                 (cons (car table)
                                                       (mapcar #'(lambda (item)
                                                                   (setf (cdadar item)
                                                                         (add-obj-to-list (build-desc-list (cdadar item))))
                                                                   (setf (cdar item)
                                                                         (add-obj-to-list (cdar item)))
                                                                   (add-obj-to-list item))
                                                               (cdr table))))
                                             request-items))))
          (when return-consumed-capacity
            (list `("ReturnConsumedCapacity" . ,return-consumed-capacity)))))

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

(defcontent query (&key table-name key-conditions return-consumed-capacity attributes-to-get index-name select
                        limit consistent-read conditional-operator exclusive-start-key expression-attribute-values
                        filter-expression projection-expression query-filter scan-index-forward)
    (table-name key-conditions)
  (append (list `("TableName" . ,table-name)
                `("KeyConditions" . ,(add-obj-to-list
                                      (mapcar #'(lambda (item)
                                                  (cons (car item)
                                                        (add-obj-to-list
                                                         (mapcar #'(lambda (prop)
                                                                     (if (string= (car prop) "AttributeValueList")
                                                                         (cons (car prop)
                                                                               (mapcar #'desc (cdr prop)))
                                                                         prop))
                                                                 (cdr item)))))
                                              key-conditions))))
          (when return-consumed-capacity
            (list `("ReturnConsumedCapacity" . ,return-consumed-capacity)))
          (when attributes-to-get
            (list `("AttributesToGet" . ,attributes-to-get)))
          (when index-name
            (list `("IndexName" . ,index-name)))
          (when select
            (list `("Select" . ,select)))
          (when limit
            (list `("Limit" . ,limit)))
          (when consistent-read
            (list `("ConsistentRead" . ,consistent-read)))
          (when conditional-operator
            (list `("ConditionalOperator" . ,conditional-operator)))
          (when exclusive-start-key
            (list `("ExclusiveStartKey" . ,(add-obj-to-list (build-desc-list exclusive-start-key)))))
          (when expression-attribute-values
            (list `("ExpressionAttributeValues" . ,(add-obj-to-list (build-desc-list expression-attribute-values)))))
          (when filter-expression
            (list `("FilterExpression" . ,filter-expression)))
          (when projection-expression
            (list `("ProjectionExpression" . ,projection-expression)))
          (when query-filter
            (list `("QueryFilter" . ,(add-obj-to-list
                                      (mapcar #'(lambda (item)
                                                  (cons (car item)
                                                        (add-obj-to-list
                                                         (mapcar #'(lambda (prop)
                                                                     (if (string= (car prop) "AttributeValueList")
                                                                         (cons (car prop)
                                                                               (mapcar #'desc (cdr prop)))
                                                                         prop))
                                                                 (cdr item)))))
                                              query-filter)))))
          (when scan-index-forward
            (list `("ScanIndexForward" . ,scan-index-forward)))))

(defcontent scan (&key table-name return-consumed-capacity attributes-to-get index-name select limit
                       consistent-read conditional-operator exclusive-start-key expression-attribute-values
                       filter-expression projection-expression scan-filter scan-index-forward segment total-segments)
    (table-name)
  (append (list `("TableName" . ,table-name))
          (when return-consumed-capacity
            (list `("ReturnConsumedCapacity" . ,return-consumed-capacity)))
          (when attributes-to-get
            (list `("AttributesToGet" . ,attributes-to-get)))
          (when index-name
            (list `("IndexName" . ,index-name)))
          (when select
            (list `("Select" . ,select)))
          (when limit
            (list `("Limit" . ,limit)))
          (when consistent-read
            (list `("ConsistentRead" . ,consistent-read)))
          (when conditional-operator
            (list `("ConditionalOperator" . ,conditional-operator)))
          (when exclusive-start-key
            (list `("ExclusiveStartKey" . ,(add-obj-to-list (build-desc-list exclusive-start-key)))))
          (when expression-attribute-values
            (list `("ExpressionAttributeValues" . ,(add-obj-to-list (build-desc-list expression-attribute-values)))))
          (when filter-expression
            (list `("FilterExpression" . ,filter-expression)))
          (when projection-expression
            (list `("ProjectionExpression" . ,projection-expression)))
          (when scan-filter
            (list `("ScanFilter" . ,(add-obj-to-list
                                     (mapcar #'(lambda (item)
                                                 (cons (car item)
                                                       (add-obj-to-list
                                                        (mapcar #'(lambda (prop)
                                                                    (if (string= (car prop) "AttributeValueList")
                                                                        (cons (car prop)
                                                                              (mapcar #'desc (cdr prop)))
                                                                        prop))
                                                                (cdr item)))))
                                             scan-filter)))))
          (when scan-index-forward
            (list `("ScanIndexForward" . ,scan-index-forward)))
          (when segment
            (list `("Segment" . ,segment)))
          (when total-segments
            (list `("TotalSegments" . ,total-segments)))))

(defcontent update-item (&key table-name key update-expression condition-expression return-values
                              expression-attribute-values expression-attribute-names return-consumed-capacity)
    (table-name key)
  (append (list `("TableName" . ,table-name)
                `("Key" . ,(add-obj-to-list (build-desc-list key))))
          (when update-expression
            (list `("UpdateExpression" . ,update-expression)))
          (when condition-expression
            (list `("ConditionExpression" . ,condition-expression)))
          (when return-values
            (list `("ReturnValues" . ,return-values)))
          (when expression-attribute-values
            (list `("ExpressionAttributeValues" . ,(add-obj-to-list (build-desc-list expression-attribute-values)))))
          (when expression-attribute-names
            (list `("ExpressionAttributeNames" . ,(add-obj-to-list expression-attribute-names))))
          (when return-consumed-capacity
            (list `("ReturnConsumedCapacity" . ,return-consumed-capacity)))))

(defcontent update-table (&key table-name attribute-definitions provisioned-throughput)
    (table-name)
  (append (list  `("TableName" . ,table-name))
          (when attribute-definitions
            (list `("AttributeDefinitions" . ,(build-obj-list attribute-definitions))))
          (when provisioned-throughput
            (list `("ProvisionedThroughput" . ,(add-obj-to-list provisioned-throughput))))))
