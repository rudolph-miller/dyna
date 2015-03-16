(in-package :cl-user)
(defpackage dyna-test.table
  (:use :cl
        :prove
        :dyna
        :dyna.column
        :dyna.table))
(in-package :dyna-test.table)

(plan nil)

(diag "dyna-test.table")

(setf (find-class 'thread nil) nil)

(ok (defclass thread ()
      ((forum-name :key-type :hash
                   :attr-name "ForumName"
                   :attr-type :S
                   :initarg :forum-name)
       (subject :key-type :range
                :attr-name "Subject"
                :attr-type :S
                :initarg :subject)
       (last-post-date-time :attr-name "LastPostDateTime"
                            :attr-type :S
                            :initarg :last-post-date-time))
      (:dyna (make-dyna :credentials (cons "DYNA_TEST_ACCESS_KEY" "DYNA_TEST_SECRET_KEY")
                        :region "local"))
      (:table-name "Thread")
      (:throuput (:write 5 :read 5))
      (:lsi last-post-date-time)
      (:gsi (:hash subject :range last-post-date-time))
      (:metaclass <dyna-table-class>))
    "can create class having <dyna-table-class> as metaclass.")

(setf (find-class 'thread2 nil) nil)

(ok (defclass thread2 ()
      ((forum-name :key-type :hash
                   :attr-type :S
                   :initarg :forum-name)
       (subject :key-type :range
                :attr-type :S
                :initarg :subject))
      (:dyna (make-dyna :credentials (cons "DYNA_TEST_ACCESS_KEY" "DYNA_TEST_SECRET_KEY")
                        :region "local"))
      (:throuput (:write 5 :read 5))
      (:metaclass <dyna-table-class>))
    "can create class having <dyna-table-class> as metaclass without :table-name.")

(let ((table (find-class 'thread))
      (table2 (find-class 'thread2)))

  (ok (find table (c2mop:class-direct-subclasses (find-class '<dyna-class>)))
      "can set <dyna-class> as one of superclasses.")

  (is-type (table-dyna table)
           'dyna
           "can handle :dyna.")

  (is (table-name table)
      "Thread"
      "can handle :table-name.")

  (is (table-name table2)
      "thread2"
      "without :table-name.")

  (is (table-throughput table)
      '(:write 5 :read 5)
      "can handle :throughput.")

  (is (attr-name (table-hash-key table))
      "ForumName"
      "can handle :key-type and :attr-name.")

  (is (attr-name (table-hash-key table2))
      "forum-name"
      "without :attr-name.")

  (is (attr-type (table-hash-key table))
      "S"
      "can handle :attr-type.")

  (subtest "table-hash-key"
    (is (attr-name (table-hash-key table))
        "ForumName"
        "can return hash-key of the table."))

  (subtest "table-hash-keys"
    (is (mapcar #'attr-name (table-hash-keys table))
        '("ForumName" "Subject")
        "can return hash-keys of the table."))

  (subtest "table-range-key"
    (is (attr-name (table-range-key table))
        "Subject"
        "can return range-keyes of the table."))

  (subtest "table-range-keys"
    (is (mapcar #'attr-name (table-range-keys table))
        '("Subject" "LastPostDateTime")
        "can return range-keyes of the table."))

  (subtest "table-lsi"
    (is-type (car (table-lsi table))
             '<dyna-table-column>
             "can return the list of #<<dyna-table-column>>."))

  (subtest "table-gsi"
    (is-type (getf (car (table-gsi table)) :hash)
             '<dyna-table-column>
             "can return the list of plist having #<<dyna-table-column>> as value.")))

(finalize)
