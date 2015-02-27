(in-package :cl-user)
(defpackage dyna-test.table-operation
  (:use :cl
        :jsown
        :prove
        :dyna-test.init
        :dyna
        :dyna.table
        :dyna.table-operation))
(in-package :dyna-test.table-operation)

(plan nil)

(diag "dyna-test.table")

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

(init-dynamo-local)

(subtest "table-exist-p"
  (defclass inexist-table ()
    ()
    (:dyna *dyna*)
    (:table-name "InexistTable")
    (:metaclass <dyna-table-class>))

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
      "Slot %synced is first NIL.")

  (setf (find-class 'thread) nil)
  (defclass thread ()
    ((forum-name :key-type :hash
                 :attr-name "ForumName"
                 :initarg :forum-name
                 :accessor thread-forum-name))
    (:dyna *dyna*)
    (:table-name "Thread")
    (:metaclass <dyna-table-class>))

  (is-error (sync-table (find-class 'thread))
            '<dyna-incompatible-table-schema>
            "can raise the error with the incompatible schema.")

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

  (ok (sync-table (find-class 'thread))
      "can return T if the table schema is correct.")

  (ok (table-synced (find-class 'thread))
      "Slot %synced is not T."))

(put-item  *dyna* :table-name "Thread"
                  :item '(("ForumName" . "Amazon DynamoDB")
                          ("Subject" . "Really useful")
                          ("Tags" . ("Multiple Items" "HelpMe"))))

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


(finalize)
