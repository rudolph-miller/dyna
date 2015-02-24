# Dyna
Dyna is a Common Lisp library for AWS DynamoDB.

## Usage

```Lisp
(let ((dyna (make-dyna :credentials (cons (asdf::getenv "AWS_ACCESS_KEY")
                                          (asdf:getenv "AWS_SECRET_KEY"))
                 :region "ap-northeast-1")))
  (put-item dyna :table-name "aliens"
                 :item (("Name" . "LispAlien") ("Feature" . "They talk Lisp.")))
  ;; => T

  (get-item dyna :table-name "aliens" :key (("Name" . "LispAlien"))))
  ;; => (("Name" . "LispAlien") ("Feature" . "They talk Lisp."))
```

## API
Most API return multiple values, the formaer is formatted result, and the latter is raw result.

### batch-get-item
```Lisp
(batch-get-item dyna :request-items '(("Forum" . (("Keys" . ((("Id" . 1))
                                                             (("Id" . 2))))
                                                  ("ProjectionExpression" . "Id, Title, Author")))
                                      ("Thread" . (("Keys" . ((("ForumName" . "Amazon DynamoDB")
                                                               ("Subject" . "Concurrent reads"))))
                                                   ("AttributesToGet" . "ForumName, Subject"))))
                     :return-consumed-capacity "TOTAL")))
=> (("Forum" (("Id" . 1) ("Title" . "Enjoy Lisp") ("Author" . "Rudolph-Miller"))
             (("Id" . 2) ("Title" . "Sophisticated Programming Language") ("Author" . "Lisp-Alien")))
    ("Thread" (("ForumName" . "Amazon DynamoDB") ("Subject" . "Concurrent reads"))))
```
  - returns a list of alists.
  - Support
    - `:request-items`
    - `:return-consumed-capacity`
### batch-write-item

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
  - Unsupport
    - `:returnItem-collection-metrics`
    - `:return-values`

### list-tables
```Lisp
(list-tables-content dyna)
;; => ("Thread")
```
  - returns a list of table names.

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

### scan

### update-item
```List
(update-item dyna :table-name "Thread"
                  :key '(("ForumName" . "Amazon DynamoDB"))
                  :update-expression "set Replies = Replies + :num"
                  :expression-attribute-values '((":num" . 1))
                  :return-values "NONE")
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

## Author

* Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## Copyright

Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## License

Licensed under the MIT License.
