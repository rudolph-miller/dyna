(in-package :cl-user)
(defpackage dyna-test.table-operation
  (:use :cl
        :jsown
        :prove
        :dyna-test.init
        :dyna
        :dyna.error
        :dyna.sxql
        :dyna.table
        :dyna.table-operation)
  (:import-from :alexandria
                :set-equal)
  (:import-from :sxql
                :where))
(in-package :dyna-test.table-operation)

(plan nil)

(diag "dyna-test.table")

(setf (find-class 'thread) nil)
(defclass thread ()
  ((forum-name :key-type :hash
               :attr-name "ForumName"
               :attr-type :S
               :initarg :forum-name
               :accessor thread-forum-name)
   (a :initarg :a
      :attr-name "a")
   (subject :key-type :range
            :attr-name "Subject"
            :attr-type :S
            :initarg :subject
            :accessor thread-subject))
  (:dyna *dyna*)
  (:table-name "Thread")
  (:throuput (:read 4 :write 5))
  (:metaclass <dyna-table-class>))

(defclass inexist-table ()
  ()
  (:dyna *dyna*)
  (:table-name "InexistTable")
  (:metaclass <dyna-table-class>))

(init-dynamo-local)

(subtest "table-exist-p"
  (ok (not (table-exist-p 'inexist-table))
      "can return NIL if table doesn't exist.")

  (ok (table-exist-p 'thread)
      "can return T if table exists."))

(subtest "describe-dyna"
  (is (filter (describe-dyna 'thread) "Table" "TableName")
      "Thread"
      "can return table-definition."))

(subtest "sync-table"
  (ok (not (table-synced (find-class 'thread)))
      "Slot %synced is at first NIL.")

  (is-error (sync-table (find-class 'inexist-table))
            '<dyna-inexist-table>
            "can raise the error with the inexisted table.")

  (setf (find-class 'thread) nil)
  (defclass thread ()
    ((forum-name :key-type :hash
                 :attr-name "ForumName"
                 :attr-type :S
                 :initarg :forum-name
                 :accessor thread-forum-name))
    (:dyna *dyna*)
    (:table-name "Thread")
    (:metaclass <dyna-table-class>))

  (is-error (sync-table (find-class 'thread))
            '<dyna-incompatible-table-schema>
            "can raise the error with the incompatible key schema.")

  (setf (find-class 'thread) nil)
  (defclass thread ()
    ((forum-name :key-type :hash
                 :attr-name "ForumName"
                 :initarg :forum-name
                 :accessor thread-forum-name)
     (subject :key-type :range
              :attr-name "Subject"
              :initarg :subject
              :accessor thread-subject))
    (:dyna *dyna*)
    (:table-name "Thread")
    (:metaclass <dyna-table-class>))

  (is-error (sync-table (find-class 'thread))
            '<dyna-incompatible-table-schema>
            "can raise the error with the incompatible attr types.")

  (setf (find-class 'thread) nil)
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
    (:throuput (:read 1 :write 1))
    (:metaclass <dyna-table-class>))

  (is-error (sync-table (find-class 'thread))
            '<dyna-incompatible-table-schema>
            "can raise the error with the incompatible throughput.")

  (setf (find-class 'thread) nil)
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
    (:throuput (:read 5 :write 5))
    (:metaclass <dyna-table-class>))

  (ok (sync-table (find-class 'thread))
      "can return T if the table schema is correct.")

  (ok (table-synced (find-class 'thread))
      "Slot %synced is now T."))

(setf (find-class 'thread) nil)
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
  (:throuput (:read 5 :write 5))
  (:metaclass <dyna-table-class>))

(subtest "migrate-dyna-table"
  (delete-table *dyna* :table-name "Thread")
  (ok (not (table-exist-p 'thread))
      "Table: Thread doesn't exist now.")

  (migrate-dyna-table 'thread)

  (ok (table-exist-p 'thread)
      "Table: Thread exists now.")

  (ok (not (migrate-dyna-table 'thread))
      "can return NIL when nothing changed.")

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
           :initarg :tags
           :accessor thread-tags))
    (:dyna *dyna*)
    (:table-name "Thread")
    (:throuput (:read 5 :write 5))
    (:metaclass <dyna-table-class>))

  (ok (not (migrate-dyna-table 'thread))
      "can return NIL when just not key slots changed.")

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
    (:throuput (:read 5 :write 3))
    (:metaclass <dyna-table-class>))

  (ok (migrate-dyna-table 'thread)
      "can update the table if the definitions are changed."))

(put-item  *dyna* :table-name "Thread"
                  :item '(("ForumName" . "Amazon DynamoDB")
                          ("Subject" . "Really useful")
                          ("Tags" . ("Multiple Items" "HelpMe"))))

(put-item  *dyna* :table-name "Thread"
                  :item '(("ForumName" . "Amazon DynamoDB")
                          ("Subject" . "Really scalable")
                          ("Tags" . ("Scalable"))))

(put-item  *dyna* :table-name "Thread"
                  :item '(("ForumName" . "Amazon RDS")
                          ("Subject" . "Scalable")
                          ("Tags" . ("How" "Easy"))))

(subtest "find-dyna"
  (let ((result (find-dyna 'thread "Amazon DynamoDB" "Really useful")))
    (is-type result
             'thread
             "can return the correct class object.")

    (is (list (thread-forum-name result) (thread-subject result))
        (list "Amazon DynamoDB" "Really useful")
        "can set values."))

  (is-error (find-dyna 'thread "Amazon DynamoDB")
            '<dyna-incomplete-argumet-error>
            "can raise the error without necessary keys.")

  (ok (not (find-dyna 'thread "NO FORUM NAME" "NO SUBJECT"))
      "can return NIL with no matching."))

(subtest "select-dyna"
  (is (mapcar #'thread-forum-name (select-dyna 'thread))
      '("Amazon RDS" "Amazon DynamoDB" "Amazon DynamoDB")
      "without args.")

  (is (mapcar #'thread-forum-name (select-dyna 'thread (where (:= :forum-name "Amazon DynamoDB"))))
      '("Amazon DynamoDB" "Amazon DynamoDB")
      "with where-clause with :=.")

  (is (mapcar #'thread-forum-name (select-dyna 'thread (where (:in :forum-name '("Amazon DynamoDB" "Amazon RDS")))))
      '("Amazon RDS" "Amazon DynamoDB" "Amazon DynamoDB")
      "with where-clause with :in.")

  (is (mapcar #'thread-forum-name (select-dyna 'thread (where (:and (:= :forum-name "Amazon DynamoDB")
                                                                    (:= :subject "Really useful")))))
      '("Amazon DynamoDB")
      "with where-clause with :and")

  (is (mapcar #'thread-forum-name (select-dyna 'thread (where (:or (:= :forum-name "Amazon DynamoDB")
                                                                   (:= :forum-name "Amazon RDS")))))
      '("Amazon RDS" "Amazon DynamoDB" "Amazon DynamoDB")
      "with where-clause with :or.")

  (is (mapcar #'thread-subject (select-dyna 'thread (where (:and (:= :forum-name "Amazon DynamoDB")
                                                                    (:or (:= :subject "Really useful")
                                                                         (:= :subject "Really scalable"))))))
      '("Really scalable" "Really useful")
      "with where-clause with nested :and and :or.")

  (is (length (select-dyna 'thread (where (:= :forum-name "Amazon DynamoDB")) :limit 1))
      1
      "with :limit.")

  (is (length (select-dyna 'thread (where (:= :forum-name "Amazon DynamoDB")) :without-continue t))
      2
      "with :without-continue.")

  (is (length (select-dyna 'thread (where (:= :forum-name "Amazon DynamoDB")) :limit 1 :with-continue t))
      2
      "with :limit and :with-continue.")

  (is (length (select-dyna 'thread (where (:= :forum-name "Amazon DynamoDB"))))
      2
      "continue without :limit or :without-continue."))

(subtest "save-dyna"
  (setf (find-class 'thread) nil)
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
     (tags :attr-name "tags"
           :initarg :tags
           :accessor thread-tags))
    (:dyna *dyna*)
    (:table-name "Thread")
    (:throuput (:read 5 :write 3))
    (:metaclass <dyna-table-class>))

  (migrate-dyna-table 'thread)

  (flet ((thread-equal (a b)
           (and (equal (thread-forum-name a)
                       (thread-forum-name b))
                (set-equal (thread-tags a) (thread-tags b) :test #'equal))))

    (let ((object (make-instance 'thread :forum-name "Amazon S3" :subject "Limiration" :tags '("AWS"))))
      (save-dyna object)

      (is (find-dyna 'thread "Amazon S3" "Limiration")
          object
          :test #'thread-equal
          "can correctly insert the object.")

      (setf (thread-tags object) '("AWS" "S3"))
      (save-dyna object)

      (is (find-dyna 'thread "Amazon S3" "Limiration")
          object
          :test #'thread-equal
          "can correctly update the object.")))

  (is-error (save-dyna (make-instance 'thread))
            '<dyna-incomplete-argumet-error>
            "can raise the error without attributes.")

  (is-error (save-dyna (make-instance 'thread :forum-name "Amazon CloudFront"))
            '<dyna-request-error>
            "can raise the error with the incompatible attributes.")

  (is-error (save-dyna (make-instance 'thread :forum-name "Amazon CloudFront" :subject 1))
            '<dyna-request-error>
            "can raise the error with the incompatible attr-type value."))

(finalize)
