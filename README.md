# Dyna - Common Lisp library for AWS DynamoDB.

## Usage

## Installation

## Supporting

- ListTables
  - returns a list of table names.

- PutItem
  - returns t if the operation succeeded.
  - Supported
    - `:table-name`
    - `:items`
    - `:on-expression`
    - `:expression-attribute-values`
  - Unsupported
    - `:expression-attribute-names`
    - `:return-consumed-capacity`
    - `:return-item-collection-metrics`
    - `:return-values`

## Author

* Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## Copyright

Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## License

Licensed under the MIT License.
