(in-package :cl-user)
(defpackage dyna-test.sxql
  (:use :cl
        :prove
        :dyna-test.init
        :dyna
        :dyna.sxql)
  (:import-from :sxql
                :where))
(in-package :dyna-test.sxql)

(plan nil)

(diag "dyna-test.sxql")

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
            :accessor thread-subject)
   (tags :attr-name "Tags"
         :accessor thread-tags))
  (:dyna *dyna*)
  (:table-name "Thread")
  (:throuput (:read 4 :write 5))
  (:metaclass <dyna-table-class>))

(subtest "queryable-op-p"
  (let ((table (find-class 'thread)))
    (ok (queryable-op-p (where (:= :forum-name "Amazon DynamoDB")) table)
        "T with hash-key.")

    (ok (queryable-op-p (where (:and (:= :forum-name "Amazon DynamoDB")
                                     (:= :subject "Really useful")))
                        table)
        "T with hash-key and range-key.")

    (ok (queryable-op-p (where (:and (:= :forum-name "Amazon DynamoDB")
                                     (:list= :tags '("AWS"))))
                        table)
        "T with non-primary-key.")

    (ok (queryable-op-p (where (:and (:= :forum-name "Amazon DynamoDB")
                                     (:and (:= :subject "Really useful")
                                           (:list= :tags '("AWS")))))
                        table)
        "T with nested :and.")

    (ok (not (queryable-op-p (where (:or (:= :forum-name "Amazon DynamoDB")
                                         (:= :subject "Really useful")))
                             table))
        "NIL with :or.")

    (ok (not (queryable-op-p (where (:and (:= :forum-name "Amazon DynamoDB")
                                          (:= :forum-name "Amazon RDS")))
                             table))
        "NIL with two hash-keys.")))

(subtest "to-key-conditions"
  (is-values (to-key-conditions (where (:= "ForumName" "Amazon RDS")) (find-class 'thread))
             '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))))
             "with :=")

  (is-values (to-key-conditions (where (:and (:= "ForumName" "Amazon RDS") (:= "Subject" "Scalable"))) (find-class 'thread))
             '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
                ("Subject" . (("AttributeValueList" . ("Scalable")) ("ComparisonOperator" . "EQ")))))
             "with :and")

  (is-values (to-key-conditions (where (:= :forum-name "Amazon RDS")) (find-class 'thread))
             '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))))
             "with slot-name")

  (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                             (:in :subject '("AWS" "Really scalable"))))
                                (find-class 'thread))
             '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ"))))
               "#filter0 IN (:filter0,:filter1)"
               (("#filter0" . "Subject"))
               ((":filter0" . "AWS") (":filter1" . "Really scalable")))
             "with op related to no primary keys."))

(subtest "to-filter-expression"
  (let ((table (find-class 'thread)))
    (is-values (to-filter-expression (where (:= :forum-name "Amazon DynamoDB")) table)
               '("#filter0 = :filter0"
                 (("#filter0" . "ForumName"))
                 ((":filter0" . "Amazon DynamoDB")))
               "with :=.")

    (is-values (to-filter-expression (where (:in :forum-name '("Amazon DynamoDB" "Amazon S3"))) table)
               '("#filter0 IN (:filter0,:filter1)"
                 (("#filter0" . "ForumName"))
                 ((":filter0" . "Amazon DynamoDB") (":filter1" . "Amazon S3")))
               "with :in.")

    (is-values (to-filter-expression (where (:list= :tags '("AWS" "HelpMe"))) table)
               '("#filter0 = :filter0"
                 (("#filter0" . "Tags"))
                 ((":filter0" . ("AWS" "HelpMe"))))
               "with :list=.")

    (is-values (to-filter-expression (where (:and (:= :forum-name "Amazon DynamoDB")
                                                  (:= :subject "Really useful")))
                                     table)
               '("(#filter0 = :filter0 AND #filter1 = :filter1)"
                 (("#filter0" . "ForumName") ("#filter1" . "Subject"))
                 ((":filter0" . "Amazon DynamoDB") (":filter1" . "Really useful")))
               "with :and.")

    (is-values (to-filter-expression (where (:or (:= :forum-name "Amazon DynamoDB")
                                                 (:= :subject "Really useful")))
                                     table)
               '("(#filter0 = :filter0 OR #filter1 = :filter1)"
                 (("#filter0" . "ForumName") ("#filter1" . "Subject"))
                 ((":filter0" . "Amazon DynamoDB") (":filter1" . "Really useful")))
               "with :or.")

    (is-values (to-filter-expression (where (:and (:or (:= :forum-name "Amazon DynamoDB")
                                                       (:= :forum-name "Amazon S3"))
                                                  (:= :subject "Really useful")))
                                     table)
               '("((#filter0 = :filter0 OR #filter0 = :filter1) AND #filter1 = :filter2)"
                 (("#filter0" . "ForumName") ("#filter1" . "Subject"))
                 ((":filter0" . "Amazon DynamoDB") (":filter1" . "Amazon S3") (":filter2" . "Really useful")))
               "with nested operations.")))

(finalize)
