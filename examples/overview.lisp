(defvar *dyna* (make-dyna :credentials (cons (asdf::getenv "AWS_ACCESS_KEY")
                                             (asdf::getenv "AWS_SECRET_KEY"))
                          :region "ap-northeast-1"))

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

(migrate-dyna-teble 'thread)
;; => T

(save-dyna (make-instance 'thread :forum-name "Amazon DynamoDB"
                                  :subject "Really useful"))
;; => T

(find-dyna 'thread "Amazon DynamoDB" "Really useful")
;; => #<THREAD :forum-name "Amazon DynamoDB" :subject "Really useful">
