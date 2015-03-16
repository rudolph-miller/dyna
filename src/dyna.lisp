(in-package :cl-user)
(defpackage dyna
  (:use :cl)
  (:import-from :dyna.error
                :<dyna-error>
                :<dyna-request-error>
                :<dyna-incomplete-argumet-error>
                :<dyna-incompatible-table-schema>
                :<dyna-changing-lsi-error>)
  (:import-from :dyna.structure
                :make-dyna
                :dyna
                :dyna-credentials
                :dyna-region)
  (:import-from :dyna.fetch
                :*local-port*
                :fetch)
  (:import-from :dyna.operation
                :batch-get-item
                :batch-write-item
                :create-table
                :delete-item
                :delete-table
                :describe-table
                :get-item
                :list-tables
                :put-item
                :query
                :scan
                :update-item
                :update-table)
  (:import-from :dyna.table
                :<dyna-table-class>
                :<dyna-class>)
  (:import-from :dyna.table-operation
                :*default-throughput*
                :describe-dyna
                :migrate-dyna-table
                :recreate-dyna-table
                :delete-dyna-table
                :find-dyna
                :select-dyna
                :save-dyna)
  (:export ;; Error
           :<dyna-error>
           :<dyna-request-error>
           :<dyna-incomplete-argumet-error>
           :<dyna-incompatible-table-schema>
           :<dyna-changing-lsi-error>

           ;; Structure
           :dyna
           :make-dyna
           :dyna-credentials
           :dyna-region

           ;; Fetch
           :*local-port*
           :fetch

           ;; Operation
           :batch-get-item
           :batch-write-item
           :create-table
           :delete-item
           :delete-table
           :describe-table
           :get-item
           :list-tables
           :put-item
           :query
           :scan
           :update-item
           :update-table

           ;; Table
           :<dyna-table-class>
           :<dyna-class>

           ;; Table Operation
           :*default-throughput*
           :describe-dyna
           :migrate-dyna-table
           :recreate-dyna-table
           :delete-dyna-table
           :find-dyna
           :select-dyna
           :save-dyna))
(in-package :dyna)

