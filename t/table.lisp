(in-package :cl-user)
(defpackage dyna-test.table
  (:use :cl
        :prove
        :dyna
        :dyna.table))
(in-package :dyna-test.table)

(plan nil)

(diag "dyna-test.table")

(setf (find-class 'thread) nil)

(ok (defclass thread ()
      ((forum-name :key-type :hash :attr-name "ForumName" :initarg :forum-name)
       (subject :key-type :range :attr-name "Subject" :initarg :subject))
      (:dyna (make-dyna :credentials (cons "DYNA_TEST_ACCESS_KEY" "DYNA_TEST_SECRET_KEY")
                        :region "local"))
      (:table-name "Thread")
      (:metaclass <dyna-table-class>))
    "can create class having <dyna-table-class> as metaclass.")

(let ((table (find-class 'thread)))

  (ok (find table (c2mop:class-direct-subclasses (find-class '<dyna-class>)))
      "can set <dyna-class> as one of superclasses.")

  (is-type (table-dyna table)
           'dyna
           "can handle :dyna.")

  (is (table-name table)
      "Thread"
      "can handle :table-name.")

  (is (attr-name (table-hash-key table))
      "ForumName"
      "can handle :key-type and :attr-name with :key-type :hash.")

  (is (attr-name (table-range-key table))
      "Subject"
      "can handle :key-type and :attr-name with :key-type :hash."))

(finalize)
