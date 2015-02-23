(in-package :cl-user)
(defpackage dyna.content
  (:use :cl
        :jsown
        :dyna.util
        :dyna.error
        :dyna)
  (:import-from :alexandria
                :plist-alist))
(in-package :dyna.content)

(syntax:use-syntax :annot)

(defmacro defcontent (name lst &body body)
  (let ((content-symbol (intern (format nil "~a-CONTENT" name))))
    `(progn (export ',content-symbol)
            (defun ,content-symbol (dyna ,@lst)
              (declare (ignorable dyna))
              (let ((result (progn ,@body)))
                (if (null result)
                    "{}"
                    (to-json result)))))))

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

(defcontent delete-item () ())

(defcontent delete-table () ())

(defcontent describe-table () ())

(defcontent get-item () ())

(defcontent list-tables ()
  nil)

(defcontent put-item (&key table-name items condition-expression expression-attribute-values)
  (unless (or table-name (dyna-table-name dyna))
    (error '<dyna-table-not-specified-error> :dyna dyna))
  `(:obj ,@(append (list  `("TableName" . ,(or table-name (dyna-table-name dyna)))
                          `("Item" . (:obj ,@(mapcar #'(lambda (pair)
                                                         (cons (car pair)
                                                               (desc (cdr pair))))
                                                     (plist-alist items)))))
                   (when condition-expression
                     (list `("ConditionExpression" . ,condition-expression)))
                   (when expression-attribute-values
                     (list `("ExpressionAttributeValues" . (:obj ,@(mapcar #'(lambda (pair)
                                                                               (cons (car pair)
                                                                                     (desc (cdr pair))))
                                                                           (plist-alist expression-attribute-values)))))))))

(defcontent query () ())

(defcontent scan () ())

(defcontent update-item () ())

(defcontent update-table () ())
