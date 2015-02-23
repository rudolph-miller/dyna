(in-package :cl-user)
(defpackage dyna-test.request
  (:use :cl
   :prove
        :dyna-test.util
   :dyna.request))
(in-package :dyna-test.request)

(plan nil)

(with-stub-now (make-clock :year 2011 :month 09 :day 09 :hour 23 :minute 36 :sec 00)
  (let ((request (make-request :method "POST"
                               :service "iam"
                               :region "us-east-1"
                               :endpoint "iam.amazonaws.com"
                               :content "Action=ListUsers&Version=2010-05-08"
                               :content-type "application/x-www-form-urlencoded; charset=utf-8"
                               :credentials '("AKIDEXAMPLE" . "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"))))
    (subtest "request-canonical-header"
      (is (request-canonical-header request)
          (format nil "~{~a~%~}"
                  (list "content-type:application/x-www-form-urlencoded; charset=utf-8"
                        "host:iam.amazonaws.com"
                        "x-amz-date:20110909T233600Z"))
          "can return correct value."))

    (subtest "request-hashed-payload"
      (is (request-hashed-payload request)
          "b6359072c78d70ebee1e81adcbab4f01bf2c23245fa365ef83fe8f1f955085e2"
          "can retur correct value."))

    (subtest "request-canonical-request"
      (is (request-canonical-request request)
          (format nil "~{~a~^~%~}"
                  (list "POST"
                        "/"
                        ""
                        "content-type:application/x-www-form-urlencoded; charset=utf-8"
                        "host:iam.amazonaws.com"
                        "x-amz-date:20110909T233600Z"
                        ""
                        "content-type;host;x-amz-date"
                        "b6359072c78d70ebee1e81adcbab4f01bf2c23245fa365ef83fe8f1f955085e2"))
          "can return correct value."))

    (subtest "request-hashed-canonical-request"
      (is (request-hashed-canonical-request request)
          "3511de7e95d28ecd39e9513b642aee07e54f4941150d8df8bf94b328ef7e55e2"
          "can return correct value."))

    (subtest "request-credential-scope"
      (is (request-credential-scope request)
          "20110909/us-east-1/iam/aws4_request"
          "can return correct value."))

    (subtest "request-signing-string"
      (is (request-signing-string request)
          (format nil "~{~a~^~%~}"
                  (list "AWS4-HMAC-SHA256"
                        "20110909T233600Z"
                        "20110909/us-east-1/iam/aws4_request"
                        "3511de7e95d28ecd39e9513b642aee07e54f4941150d8df8bf94b328ef7e55e2"))
          "can return correct value."))

    (subtest "request-signing-key"
      (is (request-signing-key request)
          #(152 241 216 137 254 196 244 66 26 220 82 43 171 12 225 248 46 105 41 194 98 237 21 229 169 76 144 239 209 227 176 231)
          :test #'equalp
          "can return correct value."))

    (subtest "request-signature"
      (is (request-signature request)
          "ced6826de92d2bdeed8f846f0bf508e8559e98e4b0199114b84c54174deb456c"
          "can return correct value."))

    (subtest "request-credential"
      (is (request-credential request)
          "AKIDEXAMPLE/20110909/us-east-1/iam/aws4_request"
          "can return correct value."))

    (subtest "request-authorization"
      (is (request-authorization request)
          (format nil "~a ~{~a~^, ~}"
                  "AWS4-HMAC-SHA256"
                  (list "Credential=AKIDEXAMPLE/20110909/us-east-1/iam/aws4_request"
                        "SignedHeaders=content-type;host;x-amz-date"
                        "Signature=ced6826de92d2bdeed8f846f0bf508e8559e98e4b0199114b84c54174deb456c"))
          "can return correct value."))))

(finalize)
