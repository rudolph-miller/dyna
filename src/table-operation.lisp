(in-package :cl-user)
(defpackage dyna.table-operation
  (:use :cl
        :dyna.error
        :dyna.operation
        :dyna.column 
        :dyna.table)
  (:import-from :jsown
                :val)
  (:import-from :closer-mop
                :class-direct-slots
                :slot-definition-name))
(in-package :dyna.table-operation)

(syntax:use-syntax :annot)

@export
(defgeneric table-exist-p (table)
  (:method ((table symbol))
    (table-exist-p (find-class table)))
  (:method ((table <dyna-table-class>))
    (if (find (table-name table) (list-tables (table-dyna table)) :test #'equal)
        t nil)))
    

@export
(defgeneric describe-dyna (table)
  (:method ((table symbol))
    (describe-dyna (find-class table)))
  (:method ((table <dyna-table-class>))
    (describe-table (table-dyna table) :table-name (table-name table))))

@export
(defgeneric sync-table (table)
  (:method ((table <dyna-table-class>))
    (when (table-synced table) (return-from sync-table t))
    (unless (table-exist-p table)
      (error '<dyna-inexist-table> :table (table-name table)))
    (let* ((table-definition (val (describe-dyna table) "Table"))
           (key-schema (val table-definition "KeySchema")))
      (unless (equal-key-schema-p key-schema table)
        (error '<dyna-incompatible-table-schema> :table (table-name table)))
      (setf (table-synced table) t))))

(defun equal-key-schema-p (schema table)
  (flet ((find-key (type)
           (let ((result (find type schema :test #'equal :key #'(lambda (obj) (val obj "KeyType")))))
             (when result (val result "AttributeName"))))
         (attr-name-if-exist (slot)
           (when slot (attr-name slot))))
    (let ((hash-key (find-key "HASH"))
          (range-key (find-key "RANGE"))
          (table-hash-key (table-hash-key table))
          (table-range-key (table-range-key table)))
      (and (equal hash-key (attr-name-if-exist table-hash-key))
           (equal range-key (attr-name-if-exist table-range-key))))))

(defun build-dyna-table-obj (table result)
  (when result
    (loop with obj = (make-instance table)
          for slot in (class-direct-slots table)
          do (setf (slot-value obj (slot-definition-name slot))
                   (cdr (assoc (attr-name slot) result :test #'equal)))
          finally (return obj))))

(defun table-projection-expression (table)
  (format nil "~{~a~^,~}"
          (mapcar #'attr-name (class-direct-slots table))))

@export
(defgeneric find-dyna (table &rest values)
  (:method ((table symbol) &rest values)
    (apply #'find-dyna (find-class table) values))
  (:method ((table <dyna-table-class>) &rest values)
    (sync-table table)
    (let ((hash-key (table-hash-key table))
          (range-key (table-range-key table)))
      (when (and range-key (null (cdr values)))
        (error '<dyna-incomplete-argumet-error> :args '(:range-key)))
      (multiple-value-bind (result raw-result error)
          (get-item (table-dyna table) :table-name (table-name table)
                                       :key (append (list (cons (attr-name hash-key) (car values)))
                                                    (when range-key
                                                      (list (cons (attr-name range-key) (cadr values)))))
                                       :return-consumed-capacity "TOTAL")
        (values (build-dyna-table-obj table result) raw-result error)))))


@export
(defgeneric select-dyna (table &rest args)
  (:method ((table symbol) &rest args)
    (apply #'select-dyna (find-class table) args))
  (:method ((table <dyna-table-class>) &rest args)
    (declare (ignore args))
    (multiple-value-bind (result raw-result error)
        (scan (table-dyna table) :table-name (table-name table)
              :projection-expression (table-projection-expression table))
      (values (mapcar #'(lambda (item)
                          (build-dyna-table-obj table item))
                      result)
              raw-result
              error))))
