(in-package :cl-user)
(defpackage dyna.sxql
  (:use :cl
        :dyna.error
        :dyna.util
        :dyna.column
        :dyna.table)
  (:import-from :sxql.sql-type
                :conjunctive-op
                :conjunctive-op-name
                :conjunctive-op-expressions
                :infix-op
                :infix-list-op
                :conjunctive-op
                :sql-op-name
                :infix-op-name
                :infix-op-left
                :infix-op-right
                :infix-list-op-name
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
                :ensure-list
                :make-keyword
                :length=
                :flatten)
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
    (error '<dyna-unsupported-op-erorr> :op obj))

  (:method ((sym sql-symbol) table)
    (let ((slot (find-slot-by-name (sql-symbol-name sym) table)))
      (if slot (attr-name slot) (sql-symbol-name sym))))

  (:method ((var sql-variable) table)
    (sql-variable-value var)))

(defun find-slot-by-name (name class)
  (find (etypecase name
          (symbol (symbol-name name))
          (string (format nil "~@:(~a~)" name)))
        (class-direct-slots class)
        :key #'(lambda (slot)
                 (symbol-name (slot-definition-name slot)))
        :test #'equal))

@export
(defgeneric queryable-op-p (op table)
  (:method ((where-clause where-clause) table)
    (queryable-op-p (where-clause-expression where-clause) table))
  (:method ((op conjunctive-op) table)
    (let ((expressions (conjunctive-op-expressions op))
          (keys (op-key op table))
          (primary-keys (table-primary-keys table)))
      (and (and-op-p op)
           (every #'(lambda (op)
                      (queryable-op-p op table))
                  expressions)
           (not (some #'conjunctive-op-p expressions))
           (length= keys (remove-duplicates keys :test #'equal))
           (include-hash-key keys table)
           (every #'(lambda (item)
                      (find item primary-keys :test #'(lambda (a b) (equal a (attr-name b)))))
                  keys))))
  (:method ((op infix-op) table)
    (if (find (op-key op table) (table-primary-keys table) :test #'(lambda (a b) (equal a (attr-name b))))
        t nil))
  (:method ((op infix-list-op) table) nil))

(defun conjunctive-op-p (op)
  (typep op 'conjunctive-op))

(defun and-op-p (op)
  (equal (conjunctive-op-name op) "AND"))

(defgeneric op-key (op table)
  (:method ((op conjunctive-op) table)
    (flatten (mapcar #'(lambda (item) (op-key item table))
                     (conjunctive-op-expressions op))))
  (:method ((op infix-op) table)
    (yield (infix-op-left op) table))

  (:method ((op infix-list-op) table)
    (yield (infix-list-op-left op) table)))

(defgeneric op-value (op table)
  (:method ((op conjunctive-op) table)
    (flatten (mapcar #'(lambda (item) (op-value item table))
                     (conjunctive-op-expressions op))))
  (:method ((op infix-op) table)
    (yield (infix-op-right op) table))

  (:method ((op infix-list-op) table)
    (mapcar #'(lambda (item) (yield item table))
            (infix-list-op-right op))))

(defun include-hash-key (keys table)
  (find (attr-name (table-hash-key table)) keys :test #'equal))

@export
(defun to-filter-expression (where-clause table)
  (let* ((expression (where-clause-expression where-clause))
         (keys (remove-duplicates (ensure-list (op-key expression table)) :test #'equal))
         (values (remove-duplicates (ensure-list (op-value expression table)) :test #'equal))
         (attr-names (gen-attr-table keys "#filter"))
         (attr-values (gen-attr-table values ":filter")))
    (values (%to-filter-expression expression attr-names attr-values table)
            attr-names
            attr-values)))

(defun %to-filter-expression (expression name-env value-env table)
  (labels ((find-attr-thing (list) (lambda (item) (car (find-if #'(lambda (pair) (equal (cdr pair) item)) list))))
           (find-attr-name (name) (funcall (find-attr-thing name-env) name))
           (find-attr-value (value) (funcall (find-attr-thing value-env) value))
           (sub (exp)
             (etypecase exp
               (conjunctive-op (format nil (format nil "(~~{~~A~~^ ~A ~~})" (conjunctive-op-name exp))
                                       (mapcar #'sub (conjunctive-op-expressions exp))))
               (infix-op (format nil "~a ~a ~a"
                                 (find-attr-name (op-key exp table))
                                 (infix-op-name exp)
                                 (find-attr-value (op-value exp table))))
               (infix-list-op (format nil "~a ~a ~a"
                                      (find-attr-name (op-key exp table))
                                      (infix-list-op-name exp)
                                      (format nil "(~{~a~^,~})" (mapcar #'find-attr-value (op-value exp table))))))))
    (sub expression)))

@export
(defgeneric to-query-expressions (op table)
  (:method ((clause where-clause) table)
    (to-query-expressions (where-clause-expression clause) table))

  (:method ((op infix-list-op) table)
    `((,(yield (infix-list-op-left op) table)
       ("AttributeValueList" . ,(mapcar #'(lambda (op) (yield op table)) (infix-list-op-right op)))
       ("ComparisonOperator"  . ,(op-comparison-name op)))))

  (:method ((op infix-op) table)
    `((,(yield (infix-op-left op) table)
       ("AttributeValueList" . ,(ensure-list (yield (infix-op-right op) table)))
       ("ComparisonOperator" . ,(op-comparison-name op)))))

  (:method ((op conjunctive-op) table)
    (mapcan #'(lambda (obj) (to-query-expressions obj table)) (conjunctive-op-expressions op))))
