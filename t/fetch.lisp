(in-package :cl-user)
(defpackage dyna-test.fetch
  (:use :cl
        :prove
        :dyna-test.init
        :dyna.fetch))
(in-package :dyna-test.fetch)

(plan nil)

(finalize)
