# Dyna

[![Build Status](https://travis-ci.org/Rudolph-Miller/dyna.svg)](https://travis-ci.org/Rudolph-Miller/dyna)

Dyna is an AWS DynamoDB ORM for Common Lisp.

## Usage

```Lisp
(defvar *dyna* (make-dyna :credentials (cons (asdf::getenv "AWS_ACCESS_KEY")
                                             (asdf::getenv "AWS_SECRET_KEY"))
                 :region "ap-northeast-1"))
 
(defclass thread ()
  ((forum-name :key-type :hash
               :attr-name "ForumName"
               :initarg :forum-name
               :accessor thread-forum-name)
   (subject :key-type :range
            :attr-name "Subject"
            :initarg :subject
            :accessor thread-subject))
  (:dyna *dyna*)
  (:table-name "Thread")
  (:metaclass <dyna-table-class>))

(migrate-dyna-teble 'thread)
;; => T

(save-dyna (make-instance 'thread :forum-name "Amazon DynamoDB"
                                  :subject "Really useful"))
;; => T

(find-dyna 'thread "Amazon DynamoDB" "Really useful")
;; => #<THREAD :forum-name "Amazon DynamoDB" :subject "Really useful">



;;; The operations below is the samples of Low Level API.

(fetch (dyna-credentials *dyna*) "local" "ListTables" "{}")
;; => #(...)

(put-item *dyna* :table-name "aliens"
                 :item (("Name" . "LispAlien") ("Feature" . "They talk Lisp.")))
;; => T

(get-item *dyna* :table-name "aliens" :key (("Name" . "LispAlien"))))
;; => (("Name" . "LispAlien") ("Feature" . "They talk Lisp."))
```

## API
Most API return multiple values, the formaer is formatted result, and the latter is raw result.

### dyna
```Lisp
(make-dyna :credentials (cons "access-key" "secret-key")
           :region "ap-northeast-1")
```
  - `dyna` object is a object for setting up.
  - `make-dyna` creates `dyna` object.
  - `:credentials` is a dotted pair of AccessKey and SecretKey.
  - `:region` is a region of your DynamoDB.
  - If you want to access you local DynamoDB Local, you can setup `:region "local"` and `(setf *local-port* 8000)`.

### <dyna-table-class>
```Lisp
(defclass thread ()
  ((forum-name :key-type :hash
               :attr-name "ForumName"
               :attr-type :S
               :initarg :forum-name
               :accessor thread-forum-name)
   (subject :key-type :range
            :attr-name "Subject"
            :attr-type :S
            :initarg :subject
            :accessor thread-subject))
  (:dyna *dyna*)
  (:table-name "Thread")
  (:throughput (:read 5 :wirte 5)
  (:metaclass <dyna-table-class>))

;; Simpler Style

(defclass thread ()
  ((forum-name :key-type :hash
               :attr-type :S
               :initarg :forum-name
               :accessor thread-forum-name)
   (subject :key-type :range
            :attr-type :S
            :initarg :subject
            :accessor thread-subject))
  (:dyna *dyna*)
  (:throughput (:read 5 :wirte 5)
  (:metaclass <dyna-table-class>))
```
  - You can create class haveing <dyna-table-class> as `:metaclass`.
  - `:dyna` can take `dyna` object.
  - `:table-name` can take table name of DynamoDB's table.(Optional.)
  - `:throughput` is the provisioned throuput of the table.
  - `:key-type` in slot should be `:hash` or `:range` and is the same as DynamoDB's table.
  - `:attr-name` in slot is AttributeName of Item in DynamoDB's table.(Optional.)
  - `:attr-type` in slot is AttributeType of Item in DynamoDB's table.(Optional without key attributes.)

### create-dyna-table
```Lisp
(create-dyna-table 'thread)
;; => T
```
  - can return T if the table is successfully created.

### update-dyna-table
```Lisp
(defclass thread ()
  ((forum-name :key-type :hash
               :attr-name "ForumName"
               :attr-type :S
               :initarg :forum-name
               :accessor thread-forum-name)
   (subject :key-type :range
            :attr-name "Subject"
            :attr-type :S
            :initarg :subject
            :accessor thread-subject))
  (:dyna *dyna*)
  (:table-name "Thread")
  (:throughput (:read 10 :wirte 10)
  (:metaclass <dyna-table-class>))
(update-dyna-table 'thread)
;; => T

(update-dyna-table 'thread)
;; => NIL
```
  - can return T if the table is successfully updated.
  - can return NIL if the table has no changs.

### migrate-dyna-table
```Lisp
(migrate-dyna-table 'thread)
=> T
```
  - can create the table if the table doesn't exist.
  - can update the table if the table definitions are chagned.
  - can return NIL if the table has no changes.

### find-dyna
```Lisp
(find-dyna 'thread "Amazon DynamoDB" "Really useful")
;; => #<THREAD :forum-name "Amazon DynamoDB" :subject "Really useful">
```
  - can return a object if matching Item exists.

### select-dyna
```Lisp
(select-dyna 'thread)
;; => (#<THREAD > <#THREAD >)

(selet-dyna 'thread (where (:= :forum-name "Amazon DynamoDB")))
;; => (#<THREAD >)

(selet-dyna 'thread (where (:or (:= :forum-name "Amazon S3")
                                (:= :forum-name "Amazon DynamoDB"))))
;; => (#<THREAD > #<THREAD >)

(selet-dyna 'thread (where (:in :forum-name '("Amazon S3" "Amazon DynamoDB"))))
;; => (#<THREAD > #<THREAD >)

(selet-dyna 'thread (where (:in :forum-name '("Amazon S3" "Amazon DynamoDB"))) :limit 1)
;; => (#<THREAD >)
```
  - returns the list of objects.
  - can handle where-clause of SxQL.
  - can handle `LastEvaluatedKey` in the response.
  - `:limit` can restrict the number of results.

### save-dyna
```Lisp
(save-dyna (make-instance 'thread :forum-name "Amazon DynamoDB" :subject "Really useful"))
;; =>
```
  - can return T if the object is successfully saved.

## Low Level API

### fetch
```Lisp
(fetch (cons "access-key" "secret-key") "ap-northeast-1" "ListTables" "{}")
;; => #(...)
```
  - returns raw octets of reponse.

### batch-get-item
```Lisp
(batch-get-item dyna :request-items '(("Forum" . (("Keys" . ((("Id" . 1))
                                                             (("Id" . 2))))
                                                  ("ProjectionExpression" . "Id, Title, Author")))
                                      ("Thread" . (("Keys" . ((("ForumName" . "Amazon DynamoDB")
                                                               ("Subject" . "Concurrent reads"))))
                                                  ("AttributesToGet" . "ForumName, Subject"))))
                     :return-consumed-capacity "TOTAL")))
;; => (("Forum" (("Id" . 1) ("Title" . "Enjoy Lisp") ("Author" . "Rudolph-Miller"))
;;              (("Id" . 2) ("Title" . "Sophisticated Programming Language") ("Author" . "Lisp-Alien")))
;;     ("Thread" (("ForumName" . "Amazon DynamoDB") ("Subject" . "Concurrent reads"))))
```
  - returns the list of alists.
  - Support
    - `:request-items`
    - `:return-consumed-capacity`

### batch-write-item
```Lisp
(batch-write-item dyna :request-items '(("Forum" . ((("PutRequest" . (("Item" . (("Name" . "Amazon DynamoDB")
                                                                                 ("Category" . "Amazon Web Services"))))))
                                                    (("PutRequest" . (("Item" . (("Name" . "Amazon RDS")
                                                                                 ("Category" . "Amazon Web Services"))))))))
                                        ("Thread" . ((("PutRequest" . (("Item" . (("ForumName" . "Amazon DynamoDB")
                                                                                  ("Subject" . "Concurrent reads")))))))))
                       :return-consumed-capacity "TOTAL")
;; => T
```
  - returns t if the operation succeeded.
  - Support
    - `:request-items`
    - `:return-consumed-capacity`

### create-table
```Lisp
(create-table dyna :table-name "Thread"
                   :key-schema '((("AttributeName" . "ForumName") ("KeyType" . "HASH"))
                                 (("AttributeName" . "Subject") ("KeyType" . "RANGE")))
                   :attribute-definitions '((("AttributeName" . "ForumName") ("AttributeType" . "S"))
                                            (("AttributeName" . "Subject") ("AttributeType" . "S"))
                                            (("AttributeName" . "LastPostDateTime") ("AttributeType" . "S")))
                   :local-secondary-indexes '((("IndexName" . "LastPostIndex")
                                               ("KeySchema" . ((("AttributeName" . "ForumName")
                                                                ("KeyType" . "HASH"))
                                                               (("AttributeName" . "LastPostDateTime")
                                                                ("KeyType" . "RANGE"))))
                                               ("Projection" . (("ProjectionType" . "KEYS_ONLY")))))
                   :provisioned-throughput '(("ReadCapacityUnits" . 5)
                                             ("WriteCapacityUnits" . 5)))
;; => T
```
  - returns t if the operation succeeded.
  - Support
    - `:table-name`
    - `:attribute-definitions`
    - `:key-schema`
    - `:global-secondary-indexes`
    - `:local-secondary-indexes`
    - `:provisioned-throughput`

### delete-item
```Lisp
(delete-item dyna :table-name "Thread"
                  :key '(("ForumName" . "Amazon DynamoDB"))
                  :condition-expression "attribute_not_exists(Replies)"
                  :return-values "ALL_OLD")
;; => T
```
  - returns t if the operation succeeded.
  - Support
    - `:table-name`
    - `:key`
    - `:condition-expression`
    - `:return-values`

### delete-table
```Lisp
(delete-table dyna :table-name "Thread")
;; => T
```
  - returns t if the operation succeeded.
  - SUpport
    - `:table-name`

### describe-table
```Lisp
(describe-table dyna :table-name "Thread")
```
  - returns the jsown object of table description.
  - Support
    - `:table-name`

### get-item
```Lisp
(get-item dyna :table-name "Thread"
               :key '(("Tags" . ("Multiple Items" "HelpMe")))
               :consistent-read t
               :return-consumed-capacity "TOTAL")
;; => (("Tags" "Multiple Items" "HelpMe") ("ForumName" . "Amazon DynamoDB"))
```
  - returns the alist of item.
  - Support
    - `:table-name`
    - `:key`
    - `:projection-expression`
    - `:consistent-read`
    - `:return-consumed-capacity`
    - `:expression-attribute-names`
  - Unsupport
    - `:returnItem-collection-metrics`
    - `:return-values`

### list-tables
```Lisp
(list-tables-content dyna)
;; => ("Thread")
```
  - returns the list of table names.

### put-item
```Lisp
(put-item dyna :table-name "Thread"
                :item '(("Tags" . ("Multiple Items" "HelpMe"))
                        ("ForumName" . "Amazon DynamoDB"))
                :condition-expression "ForumName <> :f and Subject <> :s"
                :expression-attribute-values '((":f" . "Amazon DynamoDB")
                                               (":s" . "update multiple items")))
;; => T
```
  - returns t if the operation succeeded.
  - Support
    - `:table-name`
    - `:item`
    - `:on-expression`
    - `:expression-attribute-values`
  - Unsupport
    - `:expression-attribute-names`
    - `:return-consumed-capacity`
    - `:return-item-collection-metrics`
    - `:return-values`


### query
```Lisp
(query dyna :table-name "Thread"
            :select "SPECIFIC_ATTRIBUTES"
            :attributes-to-get '("ForumName" "Subject")
            :limit 3
            :key-conditions '(("ForumName" . (("AttributeValueList" . ("Amazon DynamoDB"))
                                              ("ComparisonOperator" . "EQ"))))
            :return-consumed-capacity "TOTAL")
;; => ((("ForunName" . "Amazon DynamoDB") ("Subject" . "Concurrent reads")))
```
  - returns list of alists.
  - Support
     - `:table-name`
     - `:key-conditions`
     - `:return-consumed-capacity`
     - `:attributes-to-get`
     - `:index-name`
     - `:select`
     - `:limit`
     - `:consistent-read`
     - `:conditional-operator`
     - `:exclusive-start-key`
     - `:expression-attribute-values`
     - `:expression-attribute-names`
     - `:filter-expression`
     - `:projection-expression`
     - `:query-filter`
     - `:scan-index-forward`

### scan
```Lisp
(scan dyna :table-name "Thread"
           :projection-expression "ForumName,Subject"
           :limit 3
           :filter-expression "Replies > :num"
           :expression-attribute-values '((":num" . 10))
           :scan-index-forward t
           :return-consumed-capacity "TOTAL")
;; => ((("ForunName" . "Amazon DynamoDB") ("Subject" . "Concurrent reads")))
```
  - returns list of alists.
  - Support
     - `:table-name`
     - `:key-conditions`
     - `:return-consumed-capacity`
     - `:attributes-to-get`
     - `:index-name`
     - `:select`
     - `:limit`
     - `:consistent-read`
     - `:conditional-operator`
     - `:exclusive-start-key`
     - `:expression-attribute-values`
     - `:expression-attribute-names`
     - `:filter-expression`
     - `:projection-expression`
     - `:scan-filter`
     - `:scan-index-forward`
     - `:segment`
     - `:total-segments`

### update-item
```List
(update-item dyna :table-name "Thread"
                  :key '(("ForumName" . "Amazon DynamoDB"))
                  :update-expression "set Replies = Replies + :num"
                  :expression-attribute-values '((":num" . 1))
                  :return-values "NONE")
;; => T
```
  - returns t if the operation succeeded.
  - Support
    - `:table-name`
    - `:key`
    - `:update-expression`
    - `:expression-attribute-names`
    - `:expression-attribute-values`
    - `:return-values`
    - `:return-consumed-capacity`
  - Unsupport
    - `:return-item-collection-metrics`

### update-table
```Lisp
(update-table dyna :table-name "Thread"
                   :attribute-definitions '((("AttributeName" . "ForumName")
                                            ("AttributeType" . "S"))
                                            (("AttributeName" . "Subject")
                                            ("AttributeType" . "S"))
                                            (("AttributeName" . "LastPostDateTime")
                                            ("AttributeType" . "S")))
                   :provisioned-throughput '(("ReadCapacityUnits" . 5)
                                             ("WriteCapacityUnits" . 5)))
;; => T
```
  - returns t if the operation succeeded.
  - Support
    - `:table-name`
    - `:attribute-definitions`
    - `:provisioned-throughput`
  - Unsupport
    - `:global-secondary-indexe`

## See Also

- [SxQL](https://github.com/fukamachi/sxql/) - A SQL generator.

## Author

* Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## Copyright

Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## License

Licensed under the MIT License.
