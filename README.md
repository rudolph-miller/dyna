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

## Support

- batch-get-item

- batch-write-item

- create-table

- delete-item
  - returns t if the operation succeeded.
  - Support
    - `:table-name`
    - `:key`
    - `:condition-expression`
    - `:return-values`

- delete-table

- describe-table
  - returns the jsown object of table description.
  - Support
    - `:table-name`

- get-item
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

- ListTables
  - returns a list of table names.

- PutItem
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


- query

- scan

- update-item

- update-table

## Author

* Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## Copyright

Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## License

Licensed under the MIT License.
