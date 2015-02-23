(in-package :cl-user)
(defpackage dyna-test.content
  (:use :cl
        :prove
        :dyna
        :dyna.content))
(in-package :dyna-test.content)

(plan nil)

(diag "dyna-test.content")

(let ((dyna (make-dyna :credentials '("AKIDEXAMPLE" . "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY")
                       :table-name "sample")))

  (subtest "batch-get-item"
    (skip 1 "No tests written."))

  (subtest "batch-write-item"
    (skip 1 "No tests written."))

  (subtest "create-table"
    (skip 1 "No tests written."))

  (subtest "delete-item"
    (skip 1 "No tests written."))

  (subtest "delete-table"
    (skip 1 "No tests written."))

  (subtest "describe-table"
    (skip 1 "No tests written."))

  (subtest "get-item"
    (is (get-item-content dyna :table-name "Thread"
                               :key '(("Tags" . ("Multiple Items" "HelpMe")))
                               :consistent-read t
                               :return-consumed-capacity "TOTAL")
        (format nil "{~{~a~^,~}}"
                (list "\"TableName\":\"Thread\""
                      "\"Key\":{\"Tags\":{\"SS\":[\"Multiple Items\",\"HelpMe\"]}}"
                      "\"ConsistentRead\":true"
                      "\"ReturnConsumedCapacity\":\"TOTAL\""))
        "can return the correct JSON object."))

  (subtest "list-tables"
    (is (list-tables-content dyna)
        "{}"
        "can return the correct JSON object."))

  (subtest "put-item"
    (is (put-item-content dyna :table-name "Thread"
                               :item '(("Tags" . ("Multiple Items" "HelpMe"))
                                       ("ForumName" . "Amazon DynamoDB"))
                               :condition-expression "ForumName <> :f and Subject <> :s"
                               :expression-attribute-values '((":f" . "Amazon DynamoDB")
                                                              (":s" . "update multiple items")))
        (format nil "{~{~a~^,~}}"
                (list "\"TableName\":\"Thread\""
                      "\"Item\":{\"Tags\":{\"SS\":[\"Multiple Items\",\"HelpMe\"]},\"ForumName\":{\"S\":\"Amazon DynamoDB\"}}"
                      "\"ConditionExpression\":\"ForumName <> :f and Subject <> :s\""
                      "\"ExpressionAttributeValues\":{\":f\":{\"S\":\"Amazon DynamoDB\"},\":s\":{\"S\":\"update multiple items\"}}"))
        "can return the correct JSON object."))

  (subtest "query"
    (skip 1 "No tests written."))

  (subtest "scan"
    (skip 1 "No tests written."))

  (subtest "update-item"
    (skip 1 "No tests written."))

  (subtest "update-table"
    (skip 1 "No tests written.")))

(finalize)
