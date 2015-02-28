(in-package :cl-user)
(defpackage dyna.sxql
  (:use :cl
        :dyna.error
        :dyna.column
        :dyna.table)
  (:import-from :sxql.sql-type
                :conjunctive-op-name
                :conjunctive-op-expressions
                :infix-op
                :infix-list-op
                :conjunctive-op
                :sql-op-name
                :infix-op-left
                :infix-op-right
                :infix-list-op-left
                :infix-list-op-right
                :sql-symbol
                :sql-symbol-name
                :sql-variable
                :sql-variable-value)
  (:import-from :sxql.clause
                :where-clause
                :where-clause-expression)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :c2mop
                :class-direct-slots
                :slot-definition-name))
(in-package :dyna.sxql)

(syntax:use-syntax :annot)

(defun op-comparison-name (op)
  (let ((op-name (make-keyword (sql-op-name op))))
    (case op-name
      (:= "EQ")
      (:> "GT")
      (:>= "GE")
      (:< "LT")
      (:<= "LE")
      (:in "IN")
      (t (error '<dyna-unsupported-op-erorr> :op op)))))

@export
(defgeneric yield (obj table)
  (:method (obj table)
    (declare (ignore table))
    (error '<dyna-unsupported-op-erorr> :op obj)))

(defmethod yield ((clause where-clause) table)
  (yield (where-clause-expression clause) table))

(defmethod yield ((op infix-list-op) table)
  (cons (yield (infix-list-op-left op) table)
        (list (cons "AttributeValueList" (mapcar #'(lambda (op) (yield op table)) (infix-list-op-right op)))
              (cons "ComparisonOperator" (op-comparison-name op)))))

(defmethod yield ((op infix-op) table)
  (cons (yield (infix-op-left op) table)
        (list (cons "AttributeValueList" (list (yield (infix-op-right op) table)))
              (cons "ComparisonOperator" (op-comparison-name op)))))

(defmethod yield ((op conjunctive-op) table)
  (let ((op-name (sql-op-name op)))
    (cons op-name (mapcar #'(lambda (obj) (yield obj table)) (conjunctive-op-expressions op)))))

(defmethod yield ((sym sql-symbol) table)
  (let ((slot (find-slot-by-name (sql-symbol-name sym) table)))
    (if slot (attr-name slot) (sql-symbol-name sym))))

(defun find-slot-by-name (name class)
  (find (etypecase name
          (symbol (symbol-name name))
          (string (format nil "~@:(~a~)" name)))
        (class-direct-slots class)
        :key #'(lambda (slot)
                 (symbol-name (slot-definition-name slot)))
        :test #'equal))

(defmethod yield ((var sql-variable) table)
  (sql-variable-value var))
