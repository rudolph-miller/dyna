# Dyna - Common Lisp library for AWS DynamoDB.

## Usage

## Installation

## Support

- batch-get-item

- batch-write-item

- create-table

- delete-item

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
    - `:return-vlaues`

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
