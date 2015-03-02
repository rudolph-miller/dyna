(in-package :cl-user)
(defpackage dyna-test.expression
  (:use :cl
        :prove
        :dyna
        :dyna-test.init
        :dyna.sxql
        :dyna.expression)
  (:import-from :sxql
                :where))
(in-package :dyna-test.expression)

(plan nil)

(diag "dyna-test.expression")

(setf (find-class 'thread nil) nil)

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
  (:throuput (:read 4 :write 5))
  (:metaclass <dyna-table-class>))

(subtest "queryable-p"
  (let ((table (find-class 'thread)))
    (ok (queryable-p table (yield (where (:= :forum-name "Amazon DynamoDB")) table))
        "T with hash-key.")

    (ok (queryable-p table (yield (where (:and (:= :forum-name "Amazon DynamoDB")
                                               (:= :subject "Really useful")))
                                  table))
        "T with hash-key and range-key.")

    (ok (not (queryable-p table (yield (where (:or (:= :forum-name "Amazon DynamoDB")
                                                   (:= :subject "Really useful")))
                                       table)))
        "NIL with :or.")

    (ok (not (queryable-p table (yield (where (:and (:= :forum-name "Amazon DynamoDB")
                                                    (:= :forum-name "Amazon RDS")))
                                       table)))
        "NIL with two hash-keys.")

    (ok (not (queryable-p table (yield (where (:and (:= :forum-name "Amazon DynamoDB")
                                                    (:= :tags "AWS")))
                                       table)))
        "NIL with non-primary-key.")

    (ok (not (queryable-p table (yield (where (:and (:= :forum-name "Amazon DynamoDB")
                                                    (:and (:= :subject "Really useful")
                                                          (:= :tags "AWS"))))
                                       table)))
        "NIL with nested :and.")))

(subtest "expressions2filter-expression"
  (let ((table (find-class 'thread)))
    (is-values (expressions2filter-expression (yield (where (:= :forum-name "Amazon DynamoDB")) table))
               '("#0 = :0" (("#0" . "ForumName")) ((":0" . "Amazon DynamoDB")))
               "with :=.")

    (is-values (expressions2filter-expression (yield (where (:in :forum-name '("Amazon DynamoDB" "Amazon S3"))) table))
               '("#0 IN (:0,:1)" (("#0" . "ForumName")) ((":0" . "Amazon DynamoDB") (":1" . "Amazon S3")))
               "with :in.")

    (is-values (expressions2filter-expression (yield (where (:and (:= :forum-name "Amazon DynamoDB")
                                                                  (:= :subject "Really useful")))
                                                     table))
               '("(#0 = :0 AND #1 = :1)"
                 (("#0" . "ForumName") ("#1" . "Subject"))
                 ((":0" . "Amazon DynamoDB") (":1" . "Really useful")))
               "with :and.")

    (is-values (expressions2filter-expression (yield (where (:or (:= :forum-name "Amazon DynamoDB")
                                                                 (:= :subject "Really useful")))
                                                     table))
               '("(#0 = :0 OR #1 = :1)"
                 (("#0" . "ForumName") ("#1" . "Subject"))
                 ((":0" . "Amazon DynamoDB") (":1" . "Really useful")))
               "with :or.")

    (is-values (expressions2filter-expression (yield (where (:and (:or (:= :forum-name "Amazon DynamoDB")
                                                                       (:= :forum-name "Amazon S3"))
                                                                  (:= :subject "Really useful")))
                                                     table))
               '("((#0 = :0 OR #0 = :1) AND #1 = :2)"
                 (("#0" . "ForumName") ("#1" . "Subject"))
                 ((":0" . "Amazon DynamoDB") (":1" . "Amazon S3") (":2" . "Really useful")))
               "with nested operations.")))

(finalize)
