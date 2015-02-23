(in-package :cl-user)
(defpackage dyna.content
  (:use :cl
        :jsown
        :dyna.util
        :dyna.error
        :dyna)
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
                    (to-json (cons :obj result))))))))

(defun string-desc (str)
  `(:obj ("S" . ,str)))

(defun string-set-desc (set)
  `(:obj ("SS" . ,set)))

(defun binary-desc (binary)
  `(:obj ("B" . ,binary)))

(defun binary-set-desc (set)
  `(:obj ("BS" . ,set)))

(defun bool-desc (bool)
  (let ((bool (if bool "true" "false")))
    `(:obj ("BOOL" . ,bool))))

(defun null-desc (null)
  (declare (ignore null))
  `(:obj "NULL"))

(defun number-desc (num)
  `(:obj ("N" . ,(write-to-string num))))

(defun number-set-desc (set)
  `(:obj ("NS" . ,(mapcar #'(lambda (item) (write-to-string item)) set))))

(defun list-desc (list)
  `(:obj ("L" . ,list)))

(defun map-desc (map)
  `(:obj ("M" . ,map)))

(defun desc (object)
  (etypecase object
    (boolean (bool-desc object))
    (number (number-desc object))
    (string (string-desc object))
    (cons
     (if (every #'numberp object)
         (number-set-desc object)
         (string-set-desc object)))))

(defcontent batch-get-item () ())

(defcontent batch-write-item () ())

(defcontent create-table () ())

(defcontent delete-item (&key table-name key condition-expression return-values)
    (table-name key)
  (append (list  `("TableName" . ,table-name)
                 `("Key" . (:obj ,@(mapcar #'(lambda (pair)
                                               (cons (car pair)
                                                     (desc (cdr pair))))
                                           key))))
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
  (append (list  `("TableName" . ,(or table-name (dyna-table-name dyna)))
                 `("Key" . (:obj ,@(mapcar #'(lambda (pair)
                                               (cons (car pair)
                                                     (desc (cdr pair))))
                                           key))))
          (when projection-expression
            (list `("ProjectionExpression" . ,projection-expression)))
          (when consistent-read
            (list `("ConsistentRead" . ,consistent-read)))
          (when return-consumed-capacity
            (list `("ReturnConsumedCapacity" . ,return-consumed-capacity)))))

(defcontent list-tables () nil)

(defcontent put-item (&key table-name item condition-expression expression-attribute-values)
    (table-name item)
  (append (list  `("TableName" . ,(or table-name (dyna-table-name dyna)))
                 `("Item" . (:obj ,@(mapcar #'(lambda (pair)
                                                (cons (car pair)
                                                      (desc (cdr pair))))
                                            item))))
          (when condition-expression
            (list `("ConditionExpression" . ,condition-expression)))
          (when expression-attribute-values
            (list `("ExpressionAttributeValues" . (:obj ,@(mapcar #'(lambda (pair)
                                                                      (cons (car pair)
                                                                            (desc (cdr pair))))
                                                                  expression-attribute-values)))))))

(defcontent query () ())

(defcontent scan () ())

(defcontent update-item () ())

(defcontent update-table () ())
