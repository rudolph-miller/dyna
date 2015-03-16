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
  (:import-from :sxql.clause
                :where-clause
                :limit-clause
                :limit-clause-count1)
  (:import-from :sxql.sql-type
                :sql-variable-value)
  (:import-from :closer-mop
                :class-direct-slots
                :slot-definition-name))
(in-package :dyna.table-operation)

(syntax:use-syntax :annot)

(defvar *retry-count-table* (make-hash-table :test #'equal))

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
(defgeneric ensure-table-synced (table)
  (:method ((table symbol))
    (ensure-table-synced (find-class table)))

  (:method ((table <dyna-table-class>))
    (when (table-synced table) (return-from ensure-table-synced t))
    (unless (table-exist-p table)
      (error '<dyna-inexist-table> :table (table-name table)))
    (let* ((table-definition (val (describe-dyna table) "Table"))
           (key-schema (val table-definition "KeySchema"))
           (attr-definitions (val table-definition "AttributeDefinitions"))
           (throughput (val table-definition "ProvisionedThroughput"))
           (lsis (safety-val table-definition "LocalSecondaryIndexes"))
           (gsis (safety-val table-definition "GlobalSecondaryIndexes")))
      (unless (equal-lsi-p lsis table)
        (error '<dyna-changing-lsi-error>))
      (unless (and (equal-key-schema-p key-schema table)
                   (equal-attr-types-p attr-definitions table)
                   (equal-throughput-p throughput table)
                   (equal-gsi-p gsis table))
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
  (remove-duplicates (append (list (table-hash-key table)
                                   (table-range-key table))
                             (when (table-lsi table)
                               (table-lsi table))
                             (when (table-gsi table)
                               (mapcan #'(lambda (gsi)
                                           (append (list (getf gsi :hash))
                                                   (when (getf gsi :range) (list (getf gsi :range)))))
                                       (table-gsi table))))
                     :test #'(lambda (a b)
                               (equal (attr-name a) (attr-name b)))))

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

(defun equal-lsi-p (lsis table)
  (let ((lsi-attr-names (mapcar #'attr-name (table-lsi table))))
    (flet ((get-range-key-attr-name (obj)
             (val (find "RANGE" (val obj "KeySchema")
                        :key #'(lambda (item) (val item "KeyType"))
                        :test #'equal)
                  "AttributeName")))
      (and (length= lsis lsi-attr-names)
           (loop for item in lsis
                 always (find (get-range-key-attr-name item) lsi-attr-names :test #'equal))))))

(defun get-the-key-attr-name (obj key)
  (let ((item (find key (val obj "KeySchema")
                    :key #'(lambda (item) (val item "KeyType"))
                    :test #'equal)))
    (when item
      (val item "AttributeName"))))

(defun get-table-gsi (table gsi)
  (let ((table-gsi-list (table-gsi table)))
    (flet ((find-table-gsi (hash &optional range)
             (find (list hash range)
                   table-gsi-list
                   :test #'(lambda (a b)
                             (and (equal (car a)
                                         (attr-name (getf b :hash)))
                                  (cond ((and (cadr a) (getf b :range))
                                         (equal (cadr a)
                                                (attr-name (getf b :range))))
                                        ((not (or (cadr a) (getf b :range))) t)
                                        (t nil)))))))
      (let ((hash (get-the-key-attr-name gsi "HASH"))
            (range (get-the-key-attr-name gsi "RANGE")))
        (find-table-gsi hash range)))))

(defun equal-gsi-p (gsis table)
  (let ((table-gsi-list (table-gsi table)))
    (and (length= gsis table-gsi-list)
         (loop for gsi in gsis
               always (let* ((throughput (val gsi "ProvisionedThroughput"))
                             (read (val throughput "ReadCapacityUnits"))
                             (write (val throughput "WriteCapacityUnits"))
                             (table-gsi (get-table-gsi table gsi)))
                        (and table-gsi
                             (= read (getf table-gsi :read))
                             (= write (getf table-gsi :write))))))))

(defun build-dyna-table-obj (table result)
  (when result
    (loop with obj = (make-instance table)
          for slot in (class-direct-slots table)
          do (setf (slot-value obj (slot-definition-name slot))
                   (cdr (assoc (attr-name slot) result :test #'equal)))
          finally (return obj))))

(defun table-projection-expression (table)
  (let* ((names (mapcar #'attr-name (class-direct-slots table)))
         (table (gen-attr-table names "#projection")))
    (values (format nil "~{~a~^,~}" (mapcar #'car table))
            table)))

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
          (range-key (table-range-key table))
          (lsi (table-lsi table))
          (gsi-list (table-gsi table)))
      (create-table (table-dyna table)
          :table-name (table-name table)
        :key-schema (append `((("AttributeName" . ,(attr-name hash-key)) ("KeyType" . "HASH")))
                            (when range-key
                              `((("AttributeName" . ,(attr-name range-key)) ("KeyType" . "RANGE")))))
        :local-secondary-indexes (mapcar #'(lambda (slot)
                                             `(("IndexName" . ,(gen-lsi-name (attr-name slot)))
                                               ("KeySchema" . ((("AttributeName" . ,(attr-name hash-key))
                                                                ("KeyType" . "HASH"))
                                                               (("AttributeName" . ,(attr-name slot))
                                                                ("KeyType" . "RANGE"))))
                                               ("Projection" . (("ProjectionType" . "ALL")))))
                                         lsi)
        :global-secondary-indexes (mapcar #'(lambda (gsi)
                                              (let ((hash (getf gsi :hash))
                                                    (range (getf gsi :range))
                                                    (read (getf gsi :read))
                                                    (write (getf gsi :write)))
                                                `(("IndexName" . ,(gsi-to-index-name gsi))
                                                  ("KeySchema" . ,(append `((("AttributeName" . ,(attr-name hash))
                                                                             ("KeyType" . "HASH")))
                                                                          (when range
                                                                            `((("AttributeName" . ,(attr-name range))
                                                                               ("KeyType" . "RANGE"))))))
                                                  ("Projection" . (("ProjectionType" . "ALL")))
                                                  ("ProvisionedThroughput" . (("ReadCapacityUnits" . ,read)
                                                                              ("WriteCapacityUnits" . ,write))))))
                                          gsi-list)
        :attribute-definitions (attribute-definitions table)
        :provisioned-throughput (provisioned-throughput table)))))

@export
(defgeneric delete-dyna-table (table)
  (:method ((table symbol))
    (delete-dyna-table (find-class table)))

  (:method ((table <dyna-table-class>))
    (delete-table (table-dyna table) :table-name (table-name table))))

@export
(defgeneric recreate-dyna-table (table)
  (:method ((table symbol))
    (recreate-dyna-table (find-class table)))

  (:method ((table <dyna-table-class>))
    (delete-dyna-table table)
    (migrate-dyna-table table)))

@export
(defgeneric update-dyna-table (table)
  (:method ((table symbol))
    (update-dyna-table (find-class table)))

  (:method ((table <dyna-table-class>))
    (labels ((get-gsi (gsis table-gsi)
               (find-if #'(lambda (gsi)
                            (let ((hash (get-the-key-attr-name gsi "HASH"))
                                  (range (get-the-key-attr-name gsi "RANGE")))
                              (and (equal hash (attr-name (getf table-gsi :hash)))
                                   (or (null (getf table-gsi :range))
                                       (equal range (attr-name (getf table-gsi :range)))))))
                        gsis))
             (gsis-diff (gsis table)
               (let ((table-gsi-list (table-gsi table))
                     (created) (updated) (deleted))
                 (loop for gsi in gsis
                       for throughput = (val gsi "ProvisionedThroughput")
                       for read = (val throughput "ReadCapacityUnits")
                       for write = (val throughput "WriteCapacityUnits")
                       for got = (get-table-gsi table gsi)
                       unless got
                         do (push (let ((hash (get-the-key-attr-name gsi "HASH"))
                                        (range (get-the-key-attr-name gsi "RANGE")))
                                    `(("Delete" . (("IndexName" . ,(gen-gsi-name (append (list hash)
                                                                                         (when range (list range)))))))))
                                  deleted)
                       when (and got (not (and (= read (getf got :read))
                                               (= write (getf got :write)))))
                         do (push (let ((read (getf got :read))
                                        (write (getf got :write)))
                                    `(("Update" . (("IndexName" . ,(gsi-to-index-name got))
                                                   ("ProvisionedThroughput" . (("ReadCapacityUnits" . ,read)
                                                                               ("WriteCapacityUnits" . ,write)))))))
                                  updated))
                 (loop for table-gsi in table-gsi-list
                       unless (get-gsi gsis table-gsi)
                         do (push (let ((hash (getf table-gsi :hash))
                                        (range (getf table-gsi :range))
                                        (read (getf table-gsi :read))
                                        (write (getf table-gsi :write)))
                                    `(("Create" . (("IndexName" . ,(gsi-to-index-name table-gsi))
                                                   ("KeySchema" . ,(append `((("AttributeName" . ,(attr-name hash))
                                                                              ("KeyType" . "HASH")))
                                                                           (when range
                                                                             `((("AttributeName" . ,(attr-name range))
                                                                                ("KeyType" . "RANGE"))))))
                                                   ("Projection" . (("ProjectionType" . "ALL")))
                                                   ("ProvisionedThroughput" . (("ReadCapacityUnits" . ,read)
                                                                               ("WriteCapacityUnits" . ,write)))))))
                                  created))
                 (values created updated deleted))))
      (let* ((table-definition (val (describe-dyna table) "Table"))
             (attr-definitions (val table-definition "AttributeDefinitions"))
             (provisioned-throughput (val table-definition "ProvisionedThroughput"))
             (gsis (safety-val table-definition "GlobalSecondaryIndexes"))
             (attr-types-changed-p (not (equal-attr-types-p attr-definitions table)))
             (throughput-changed-p (not (equal-throughput-p provisioned-throughput table)))
             (gsi-changed-p (not (equal-gsi-p gsis table)))
             (gsis-diff (when gsi-changed-p (multiple-value-list (gsis-diff gsis table)))))
        (when (or attr-types-changed-p throughput-changed-p gsi-changed-p)
          (update-table (table-dyna table)
                        :table-name (table-name table)
                        :attribute-definitions (when (or attr-types-changed-p gsi-changed-p)
                                                 (attribute-definitions table))
                        :provisioned-throughput (when throughput-changed-p (provisioned-throughput table))
                        :global-secondary-index-updates (when gsi-changed-p (reduce #'append gsis-diff))))))))

@export
(defgeneric find-dyna (table &rest values)
  (:method ((table symbol) &rest values)
    (apply #'find-dyna (find-class table) values))

  (:method ((table <dyna-table-class>) &rest values)
    (ensure-table-synced table)
    (let ((hash-key (table-hash-key table))
          (range-key (table-range-key table)))
      (when (and range-key (null (cdr values)))
        (error '<dyna-incomplete-argumet-error> :args '(:range-key)))
      (multiple-value-bind (result raw-result)
          (multiple-value-bind (projection attr-names) (table-projection-expression table)
            (get-item (table-dyna table) :table-name (table-name table)
                                         :key (append (list (cons (attr-name hash-key) (car values)))
                                                      (when range-key
                                                        (list (cons (attr-name range-key) (cadr values)))))
                                         :projection-expression projection
                                         :expression-attribute-names attr-names
                                         :return-consumed-capacity "TOTAL"))
        (values (build-dyna-table-obj table result) raw-result)))))

@export
(defgeneric select-dyna (table &rest args)
  (:method ((table symbol) &rest args)
    (apply #'select-dyna (find-class table) args))

  (:method ((table <dyna-table-class>) &rest args)
    (ensure-table-synced table)
    (let* ((where-clause (when (and args (typep (car args) 'where-clause))
                           (pop args)))
           (limit (or (when (and args (typep (car args) 'limit-clause))
                        (sql-variable-value (limit-clause-count1 (pop args))))
                      (getf args :limit)))
           (start-key (getf args :start-key))
           (last-result (getf args :last-result))
           (with-continue (getf args :with-continue))
           (without-continue (getf args :without-continue))
           (use-query (getf args :use-query))
           (queryable (or (getf args :queryable)
                          (and where-clause (queryable-op-p where-clause table))))
           (segment (getf args :segment))
           (segments (getf args :segments)))
      (flet ((get-result (&optional segment)
               (multiple-value-bind (result raw-result)
                   (if (or use-query queryable)
                       (query-dyna table where-clause :start-key start-key :limit limit)
                       (scan-dyna table where-clause :start-key start-key :limit limit :segment segment :segments segments))
                 (let ((last-evaluated-key (safety-val raw-result "LastEvaluatedKey")))
                   (if (and (not without-continue) (or (not limit) with-continue) last-evaluated-key)
                       (apply #'select-dyna table
                              (append (when where-clause (list where-clause))
                                      (list :queryable queryable
                                            :last-result (append last-result result)
                                            :start-key last-evaluated-key
                                            :with-continue t
                                            :limit limit
                                            :use-query (or use-query queryable)
                                            :segment segment
                                            :segments segments)))
                       (values (mapcar #'(lambda (item)
                                           (build-dyna-table-obj table item))
                                       (append last-result result))
                               raw-result))))))
        (if (and (not queryable) segments)
            (if segment
                (get-result segment)
                (loop for i from 0 below segments
                      for result = (get-result i)
                      when result nconc result))
            (get-result))))))

(defun scan-dyna (table where-clause &key start-key limit segment segments)
  (multiple-value-bind (expression filter-attr-names attr-values)
      (when where-clause (to-filter-expression where-clause table))
    (multiple-value-bind (projection projection-attr-names) (table-projection-expression table)
      (scan (table-dyna table)
            :table-name (table-name table)
            :select "SPECIFIC_ATTRIBUTES"
            :limit limit
            :exclusive-start-key start-key
            :projection-expression projection
            :filter-expression expression
            :expression-attribute-names (append filter-attr-names projection-attr-names)
            :expression-attribute-values attr-values
            :segment segment
            :total-segments segments))))

(defun query-dyna (table where-clause &key start-key limit)
  (multiple-value-bind (key-conditions index-name filter-expression filter-attr-names attr-values)
      (to-key-conditions where-clause table)
    (multiple-value-bind (projection projection-attr-names) (table-projection-expression table)
      (query (table-dyna table)
             :table-name (table-name table)
             :limit limit
             :exclusive-start-key start-key
             :key-conditions key-conditions
             :index-name index-name
             :projection-expression projection
             :filter-expression filter-expression
             :expression-attribute-names (append filter-attr-names projection-attr-names)
             :expression-attribute-values attr-values))))

@export
(defgeneric save-dyna (obj)
  (:method ((obj <dyna-class>))
    (let ((table (class-of obj)))
      (ensure-table-synced table)
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
        (ensure-table-synced table)
        (delete-item (table-dyna table)
                     :table-name (table-name table)
                     :key (append (list (cons (attr-name hash-key)
                                              (get-value-if-bounded obj (slot-definition-name hash-key))))
                                  (when range-key
                                    (list (cons (attr-name range-key)
                                                (get-value-if-bounded obj (slot-definition-name range-key)))))))))))
