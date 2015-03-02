(in-package :cl-user)
(defpackage dyna.expression
  (:use :cl
        :dyna.column
        :dyna.table
        :dyna.sxql)
  (:import-from :alexandria
                :flatten
                :length=
                :make-keyword))
(in-package :dyna.expression)

(syntax:use-syntax :annot)

(defun conj-p (expressions)
  (let ((car (car expressions)))
    (or (equal car "AND") (equal car "OR"))))

(defun expression-keys (expressions)
  (flatten
   (if (conj-p expressions)
       (mapcar #'expression-keys (cdr expressions))
       (car expressions))))

(defun expression-attr-value-list (expressions)
  (cdr (assoc "AttributeValueList" (cdr expressions) :test #'equal)))

(defun expression-values (expressions)
  (flatten (if (conj-p expressions)
               (mapcar #'expression-values (cdr expressions))
               (expression-attr-value-list expressions))))

(defun expression-operator (expressions)
  (cdr (assoc "ComparisonOperator" (cdr expressions) :test #'equal)))

(defun include-hash-key (keys table)
  (find (attr-name (table-hash-key table)) keys :test #'equal))

@export
(defun queryable-p (table expressions)
  (let ((keys (expression-keys expressions)))
    (when (include-hash-key keys table)
      (cond
        ((and (not (conj-p expressions))
              (equal (expression-operator expressions) "EQ")) t)
        ((and (conj-p expressions) (not (some #'conj-p (cdr expressions))))
         (when (not (equal (car expressions) "OR"))
           (let* ((duplicated-p (not (length= keys (remove-duplicates keys :test #'equal))))
                  (primary-keys (mapcar #'attr-name (table-primary-keys table)))
                  (all-primary-keys-p (every #'(lambda (item)
                                                 (find item primary-keys :test #'equal))
                                             keys)))
             (and (not duplicated-p) all-primary-keys-p))))
        (t nil)))))

(defun gen-attr-table (list pre)
  (loop for key in list
        for i from 0
        collecting (cons (format nil "~a~a" pre i) key)))

@export
(defun expressions2filter-expression (expressions)
  (let* ((keys (remove-duplicates (expression-keys expressions) :test #'equal))
         (values (remove-duplicates (expression-values expressions) :test #'equal))
         (attr-names (gen-attr-table keys #\#))
         (attr-values (gen-attr-table values #\:)))
        (values (%expressions2filter-expression expressions attr-names attr-values)
                attr-names
                attr-values)))

(defun %expressions2filter-expression (expressions attr-name-env attr-value-env)
  (labels ((find-attr-thing (list) (lambda (item) (car (find-if #'(lambda (pair) (equal (cdr pair) item)) list))))
           (find-attr-name (name) (funcall (find-attr-thing attr-name-env) name))
           (find-attr-value (value) (funcall (find-attr-thing attr-value-env) value))
           (sub (exps)
             (if (conj-p exps)
                 (format nil (format nil "(~~{~~A~~^ ~A ~~})" (car exps))
                         (mapcar #'sub (cdr exps)))
                 (format nil "~a ~a ~a"
                         (find-attr-name (car exps))
                         (format-operator (expression-operator exps))
                         (let ((values (expression-attr-value-list exps)))
                           (if (null (cdr values))
                               (find-attr-value (car values))
                               (format nil "(~{~a~^,~})" (mapcar #'find-attr-value values))))))))
    (sub expressions)))

(defun format-operator (operator)
  (ecase (make-keyword operator)
    (:eq "=")
    (:gt ">")
    (:ge ">=")
    (:lt "<")
    (:le "<=")
    (:in "IN")))
