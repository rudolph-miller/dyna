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
            :accessor thread-subject))
  (:dyna *dyna*)
  (:table-name "Thread")
  (:throuput (:read 4 :write 5))
  (:metaclass <dyna-table-class>))

(is (yield (where (:= "ForumName" "Amazon RDS")) (find-class 'thread))
    '("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
    "with :=")

(is (yield (where (:> "ForumName" "Amazon RDS")) (find-class 'thread))
    '("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "GT")))
    "with :>")

(is (yield (where (:>= "ForumName" "Amazon RDS")) (find-class 'thread))
    '("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "GE")))
    "with :>=")

(is (yield (where (:< "ForumName" "Amazon RDS")) (find-class 'thread))
    '("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "LT")))
    "with :<")

(is (yield (where (:<= "ForumName" "Amazon RDS")) (find-class 'thread))
    '("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "LE")))
    "with :<=")

(is (yield (where (:in "ForumName" '("Amazon RDS"))) (find-class 'thread))
    '("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "IN")))
    "with :in")

(is (yield (where (:and (:= "ForumName" "Amazon RDS") (:= "Subject" "Scalable"))) (find-class 'thread))
    '("AND" ("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
      ("Subject" . (("AttributeValueList" . ("Scalable")) ("ComparisonOperator" . "EQ"))))
    "with :and")

(is (yield (where (:or (:= "ForumName" "Amazon RDS") (:= "Subject" "Scalable"))) (find-class 'thread))
    '("OR" ("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
      ("Subject" . (("AttributeValueList" . ("Scalable")) ("ComparisonOperator" . "EQ"))))
    "with :or")

(is (yield (where (:= :forum-name "Amazon RDS")) (find-class 'thread))
    '("ForumName" . (("AttributeValueList" . ("Amazon RDS")) ("ComparisonOperator" . "EQ")))
    "with slot-name")

(finalize)
