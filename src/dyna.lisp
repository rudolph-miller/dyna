(in-package :cl-user)
(defpackage dyna
  (:use :cl
        :jsown
        :dyna.error
        :dyna.util
        :dyna.content)
  (:import-from :dyna.error
                :<dyna-error>
                :<dyna-request-error>
                :<dyna-incomplete-argumet-error>
                :<dyna-incomplete-argumet-error>)
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
  (:export ;; Error
           :<dyna-error>
           :<dyna-request-error>
           :<dyna-incomplete-argumet-error>
           :<dyna-incompatible-table-schema>

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
           :update-table))
(in-package :dyna)

