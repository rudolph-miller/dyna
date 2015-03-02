(in-package :cl-user)
(defpackage dyna.table-operation
  (:use :cl
        :dyna.util
        :dyna.error
        :dyna.operation
        :dyna.column 
        :dyna.table
        :dyna.sxql)
  (:import-from :jsown
                :val)
  (:import-from :alexandria
                :flatten
                :length=)
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
  (:method ((table symbol))
    (sync-table (find-class table)))
  (:method ((table <dyna-table-class>))
    (when (table-synced table) (return-from sync-table t))
    (unless (table-exist-p table)
      (error '<dyna-inexist-table> :table (table-name table)))
    (let* ((table-definition (val (describe-dyna table) "Table"))
           (key-schema (val table-definition "KeySchema"))
           (attr-definitions (val table-definition "AttributeDefinitions"))
           (throughput (val table-definition "ProvisionedThroughput")))
      (unless (and (equal-key-schema-p key-schema table)
                   (equal-attr-types-p attr-definitions table)
                   (equal-throughput-p throughput table))
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

(defun table-should-define-slots (table)
  (remove-if-not #'(lambda (slot)
                     (key-type slot))
                 (class-direct-slots table)))

(defun equal-attr-types-p (schema table)
  (flet ((find-attr-type (name)
           (let ((result (find name schema :test #'equal :key #'(lambda (obj) (val obj "AttributeName")))))
             (when result (val result "AttributeType")))))
    (let ((slots (table-should-define-slots table)))
      (and (length= slots schema)
           (loop for slot in slots
                 always (and (slot-boundp slot 'attr-type)
                             (equal (attr-type slot) (find-attr-type (attr-name slot)))))))))

(defun equal-throughput-p (schema table)
  (and (= (val schema "ReadCapacityUnits") (getf (table-throughput table) :read))
       (= (val schema "WriteCapacityUnits") (getf (table-throughput table) :write))))

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
(defgeneric migrate-dyna-table (table)
  (:method ((table symbol))
    (migrate-dyna-table (find-class table)))
  (:method ((table <dyna-table-class>))
    (if (table-exist-p table)
        (update-dyna-table table)
        (create-dyna-table table))))

(defun attribute-definitions (table)
  (loop for slot in (table-should-define-slots table)
        collecting `(("AttributeName" . ,(attr-name slot))
                     ("AttributeType" . ,(attr-type slot)))))

(defun provisioned-throughput (table)
  (let ((throughput (table-throughput table)))
    `(("ReadCapacityUnits" . ,(getf throughput :read))
      ("WriteCapacityUnits" . ,(getf throughput :write)))))

@export
(defgeneric create-dyna-table (table)
  (:method ((table symbol))
    (create-dyna-table (find-class table)))
  (:method ((table <dyna-table-class>))
    (let ((hash-key (table-hash-key table))
          (range-key (table-range-key table)))
      (create-table (table-dyna table)
                    :table-name (table-name table)
                    :key-schema (append `((("AttributeName" . ,(attr-name hash-key)) ("KeyType" . "HASH")))
                                        (when range-key
                                          `((("AttributeName" . ,(attr-name range-key)) ("KeyType" . "RANGE")))))
                    :attribute-definitions (attribute-definitions table)
                    :provisioned-throughput (provisioned-throughput table)))))

@export
(defgeneric update-dyna-table (table)
  (:method ((table symbol))
    (update-dyna-table (find-class table)))
  (:method ((table <dyna-table-class>))
    (let* ((table-definition (val (describe-dyna table) "Table"))
           (attr-definitions (val table-definition "AttributeDefinitions"))
           (provisioned-throughput (val table-definition "ProvisionedThroughput"))
           (attr-types-changed-p (not (equal-attr-types-p attr-definitions table)))
           (throughput-changed-p (not (equal-throughput-p provisioned-throughput table))))
      (when (or attr-types-changed-p throughput-changed-p)
        (update-table (table-dyna table)
                      :table-name (table-name table)
                      :attribute-definitions (unless (equal-attr-types-p attr-definitions table)
                                               (attribute-definitions table))
                      :provisioned-throughput (unless (equal-throughput-p provisioned-throughput table)
                                                (provisioned-throughput table)))))))

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
      (multiple-value-bind (result raw-result)
          (get-item (table-dyna table) :table-name (table-name table)
                                       :key (append (list (cons (attr-name hash-key) (car values)))
                                                    (when range-key
                                                      (list (cons (attr-name range-key) (cadr values)))))
                                       :return-consumed-capacity "TOTAL")
        (values (build-dyna-table-obj table result) raw-result)))))

@export
(defgeneric select-dyna (table &optional where-clause)
  (:method ((table symbol) &optional where-clause)
    (select-dyna (find-class table) where-clause))
  (:method ((table <dyna-table-class>) &optional where-clause)
    (let ((expressions (when where-clause (yield where-clause table))))
      (multiple-value-bind (result raw-result)
          (if (queryable-p table expressions)
              (query-dyna table expressions)
              (scan-dyna table expressions))
        (values (mapcar #'(lambda (item)
                            (build-dyna-table-obj table item))
                        result)
                raw-result)))))

(defun queryable-p (table expressions)
  (let ((keys (expression-keys expressions)))
    (when (has-hash-key keys table)
      (cond
        ((not (conj-p expressions)) t)
        ((not (some #'conj-p (cdr expressions)))
         (when (not (equal (car expressions) "OR"))
           (let* ((duplicated-p (not (length= keys (remove-duplicates keys :test #'equal))))
                  (primary-keys (mapcar #'attr-name (table-primary-keys table)))
                  (all-primary-keys-p (every #'(lambda (item)
                                                 (find item primary-keys :test #'equal))
                                             keys)))
             (and (not duplicated-p) all-primary-keys-p))))
        (t nil)))))

(defun has-hash-key (keys table)
  (find (attr-name (table-hash-key table)) keys :test #'equal))

(defun expression-keys (expressions)
  (flatten
   (if (conj-p expressions)
       (mapcar #'expression-keys (cdr expressions))
       (car expressions))))

(defun conj-p (expressions)
  (let ((car (car expressions)))
    (or (equal car "AND") (equal car "OR"))))

(defun scan-dyna (table expressions)
  (scan (table-dyna table)
        :table-name (table-name table)
        :select "SPECIFIC_ATTRIBUTES"
        :projection-expression (table-projection-expression table)))

(defun query-dyna (table expressions)
  (let ((expressions (cond
                       ((equal (car expressions) "AND") (cdr expressions))
                       ((equal (car expressions) "OR") (error '<dyna-unsupported-op-erorr> :op expressions))
                       (t (list expressions)))))
    (query (table-dyna table)
           :table-name (table-name table)
           :key-conditions expressions
           :projection-expression (table-projection-expression table))))

@export
(defgeneric save-dyna (obj)
  (:method ((obj <dyna-class>))
    (let ((table (class-of obj)))
      (sync-table table)
      (put-item (table-dyna table)
                :table-name (table-name table)
                :item (loop for slot in (class-direct-slots table)
                            for name = (slot-definition-name slot)
                            when (slot-boundp obj name)
                              collecting (cons (attr-name slot)
                                               (slot-value obj name)))))))


@export
(defgeneric delete-dyna (obj)
  (:method ((obj <dyna-class>))
    (flet ((get-value-if-bounded (object slot)
             (when (slot-boundp object slot) (slot-value object slot))))
      (let* ((table (class-of obj))
             (hash-key (table-hash-key table))
             (range-key (table-range-key table)))
        (sync-table table)
        (delete-item (table-dyna table)
                     :table-name (table-name table)
                     :key (append (list (cons (attr-name hash-key)
                                              (get-value-if-bounded obj (slot-definition-name hash-key))))
                                  (when range-key
                                    (list (cons (attr-name range-key)
                                                (get-value-if-bounded obj (slot-definition-name range-key)))))))))))
