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
   (last-post-date-time :key-type :range
                        :attr-name "LastPostDateTime"
                        :attr-type :S
                        :initarg :last-post-date-time
                        :accessor thread-last-post-date-time)
   (owner :attr-name "Owner"
          :attr-type :S
          :accessor thread-owner)
   (tags :attr-name "Tags"
         :attr-type :SS
         :accessor thread-tags))
  (:dyna *dyna*)
  (:table-name "Thread")
  (:throughput (:read 5 :write 5))
  (:lsi last-post-date-time tags)
  (:gsi (:hash owner :range last-post-date-time :read 5 :write 5))
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

    (ok (not (queryable-op-p (where (:in :forum-name '("Amazon RDS" "Amazon DynamoDB"))) table))
        "NIL with :in")
    
    (ok (not (queryable-op-p (where (:between :forum-name '("Amazon RDS" "Amazon DynamoDB"))) table))
        "NIL with :between")
    
    (ok (not (queryable-op-p (where (:is-null :forum-name)) table))
        "NIL with :is-null")
    
    (ok (not (queryable-op-p (where (:not-null :forum-name)) table))
        "NIL with :not-null")
    
    (ok (not (queryable-op-p (where (:begins-with :forum-name "Amazon")) table))
        "NIL with :begins-with")

    (ok (not (queryable-op-p (where (:contains :forum-name "Amazon")) table))
        "NIL with :contains")
    
    (ok (not (queryable-op-p (where (:or (:= :forum-name "Amazon DynamoDB")
                                         (:= :subject "Really useful")))
                             table))
        "NIL with :or.")

    (ok (queryable-op-p (where (:= :owner "Rudolph"))
                        table)
        "T with gsi.")

    (ok (queryable-op-p (where (:and (:= :owner "Rudolph")
                                     (:list= :tags '("AWS"))))
                        table)
        "T and :and with gsi.")))

(subtest "to-key-conditions"
  (let ((table (find-class 'thread)))
    (is-values (to-key-conditions (where (:= "ForumName" "Amazon RDS"))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ"))))
                 nil)
               "with :=.")

    (is-values (to-key-conditions (where (:> "ForumName" "Amazon RDS"))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "GT"))))
                 nil)
               "with other than :=.")

    (is-values (to-key-conditions (where (:and (:= "ForumName" "Amazon RDS")
                                               (:= "Subject" "Scalable")))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
                  ("Subject" . (("AttributeValueList" . ("Scalable")) ("ComparisonOperator" . "EQ"))))
                 nil)
               "with :and.")

    (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                               (:in :subject '("AWS" "Really scalable"))))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ"))))
                 nil
                 "#filter0 IN (:filter0,:filter1)"
                 (("#filter0" . "Subject"))
                 ((":filter0" . "AWS") (":filter1" . "Really scalable")))
               "with :in.")

    (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                               (:list= :tags '("Easy" "Scalable"))))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
                  ("Tags" . (("AttributeValueList" . ("Easy" "Scalable")) ("ComparisonOperator" . "EQ"))))
                 "Local-Tags-Index")
               "with :list=.")

    (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                               (:list-in :tags '(("AWS") ("Easy" "Scalable")))))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ"))))
                 nil
                 "#filter0 IN (:filter0,:filter1)"
                 (("#filter0" . "Tags"))
                 ((":filter0" . ("AWS")) (":filter1" . ("Easy" "Scalable"))))
               "with :list-in.")

    (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                               (:between :subject '("a" "z"))))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
                  ("Subject" . (("AttributeValueList" . ("a" "z")) ("ComparisonOperator" . "BETWEEN"))))
                 nil)
               "with :between.")

    (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                               (:begins-with :subject "AWS")))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
                  ("Subject" . (("AttributeValueList" . ("AWS")) ("ComparisonOperator" . "BEGINS_WITH"))))
                 nil)
               "with :begins-with.")

    (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                               (:contains :subject "AWS")))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ"))))
                 nil
                 "contains(#filter0, :filter0)"
                 (("#filter0" . "Subject"))
                 ((":filter0" . "AWS")))
               "with :contains.")

    (is-values (to-key-conditions (where (:= :forum-name "Amazon RDS"))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ"))))
                 nil)
               "with slot-name.")

    (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                               (:= :subject "AWS")
                                               (:= :last-post-date-time "2014-12-25")))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
                  ("Subject" . (("AttributeValueList" . ("AWS")) ("ComparisonOperator" . "EQ"))))
                 nil
                 "#filter0 = :filter0"
                 (("#filter0" . "LastPostDateTime"))
                 ((":filter0" . "2014-12-25")))
               "with more than one range keys.")

    (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                               (:= :last-post-date-time "2014-12-25")))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
                  ("LastPostDateTime" . (("AttributeValueList" . ("2014-12-25")) ("ComparisonOperator" . "EQ"))))
                 "Local-LastPostDateTime-Index")
               "with a local-index.")

    (is-values (to-key-conditions (where (:and (:= :forum-name "Amazon RDS")
                                               (:= :last-post-date-time "2014-12-25")
                                               (:list= :tags '("Scalable" "Easy"))))
                                  table)
               '((("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
                  ("LastPostDateTime" . (("AttributeValueList" . ("2014-12-25")) ("ComparisonOperator" . "EQ"))))
                 "Local-LastPostDateTime-Index"
                 "#filter0 = :filter0"
                 (("#filter0" . "Tags"))
                 ((":filter0" "Scalable" "Easy")))
               "with more than one lsi.")

    (is-values (to-key-conditions (where (:= :owner "Rudolph"))
                                  table)
               '((("Owner" . (("AttributeValueList" . ("Rudolph")) ("ComparisonOperator" . "EQ"))))
                 "Global-Owner-LastPostDateTime-Index")
               "with hash-key of gsi.")

    (is-values (to-key-conditions (where (:and (:= :owner "Rudolph")
                                               (:= :last-post-date-time "2014-12-25")))
                                  table)
               '((("Owner" . (("AttributeValueList" . ("Rudolph")) ("ComparisonOperator" . "EQ")))
                  ("LastPostDateTime" . (("AttributeValueList" . ("2014-12-25")) ("ComparisonOperator" . "EQ"))))
                 "Global-Owner-LastPostDateTime-Index")
               "with hash-key and range-key of gsi.")

    (is-values (to-key-conditions (where (:and (:= :owner "Rudolph")
                                               (:list= :tags '("Easy"))))
                                  table)
               '((("Owner" . (("AttributeValueList" . ("Rudolph")) ("ComparisonOperator" . "EQ"))))
                 "Global-Owner-LastPostDateTime-Index"
                 "#filter0 = :filter0"
                 (("#filter0" . "Tags"))
                 ((":filter0" . ("Easy"))))
               "with gis and :and.")))

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

    (is-values (to-filter-expression (where (:list-in :tags '(("AWS") ("Easy" "Scalable")))) table)
               '("#filter0 IN (:filter0,:filter1)"
                 (("#filter0" . "Tags"))
                 ((":filter0" . ("AWS")) (":filter1" . ("Easy" "Scalable"))))
               "with :list-in.")

    (is-values (to-filter-expression (where (:between :tags '("a" "z"))) table)
               '("#filter0 BETWEEN :filter0 AND :filter1"
                 (("#filter0" . "Tags"))
                 ((":filter0" . "a") (":filter1" . "z")))
               "with :between.")

    (is-values (to-filter-expression (where (:is-null :tags)) table)
               '("attribute_not_exists( #filter0 )"
                 (("#filter0" . "Tags"))
                 nil)
               "with :is-null.")

    (is-values (to-filter-expression (where (:not-null :tags)) table)
               '("attribute_exists( #filter0 )"
                 (("#filter0" . "Tags"))
                 nil)
               "with :not-null.")

    (is-values (to-filter-expression (where (:begins-with :forum-name "Amazon")) table)
               '("begins_with(#filter0, :filter0)"
                 (("#filter0" . "ForumName"))
                 ((":filter0" . "Amazon")))
               "with :begins-with.")

    (is-values (to-filter-expression (where (:contains :forum-name "Amazon")) table)
               '("contains(#filter0, :filter0)"
                 (("#filter0" . "ForumName"))
                 ((":filter0" . "Amazon")))
               "with :contains.")

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
