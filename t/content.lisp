(in-package :cl-user)
(defpackage dyna-test.content
  (:use :cl
        :prove
        :dyna-test.init
        :dyna
        :dyna.error
        :dyna.content))
(in-package :dyna-test.content)

(plan 13)

(diag "dyna-test.content")

(let ((dyna (make-dyna :credentials '("AKIDEXAMPLE" . "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"))))

  (subtest "batch-get-item"
    (is-error (batch-get-item-content dyna :return-consumed-capacity "TOTAL")
              '<dyna-incomplete-argumet-error>
              "can raise the error without :request-items.")
    (is (batch-get-item-content dyna :request-items '(("Forum" . (("Keys" . ((("Name" . "Amazon DynamoDB"))
                                                                             (("Name" . "Amazon RDS"))
                                                                             (("Name" . "Amazon Redshift"))))
                                                                  ("ProjectionExpression" . "Id, ISBN, Title, Authors")))
                                                      ("Thread" . (("Keys" . ((("ForumName" . "Amazon DynamoDB")
                                                                               ("Subject" . "Concurrent reads"))))
                                                                   ("AttributesToGet" . "ForumName,Subject"))))
                                     :return-consumed-capacity "TOTAL")
        (build-json "\"RequestItems\":{\"Forum\":{\"Keys\":[{\"Name\":{\"S\":\"Amazon DynamoDB\"}},{\"Name\":{\"S\":\"Amazon RDS\"}},{\"Name\":{\"S\":\"Amazon Redshift\"}}],\"ProjectionExpression\":\"Id, ISBN, Title, Authors\"},\"Thread\":{\"Keys\":[{\"ForumName\":{\"S\":\"Amazon DynamoDB\"},\"Subject\":{\"S\":\"Concurrent reads\"}}],\"AttributesToGet\":\"ForumName,Subject\"}}"
                    "\"ReturnConsumedCapacity\":\"TOTAL\"")
        "can return the correct JSON object."))

  (subtest "batch-write-item"
    (is-error (batch-write-item-content dyna :return-consumed-capacity "TOTAL")
              '<dyna-incomplete-argumet-error>
              "can raise the error without :request-items.")
    (is (batch-write-item-content dyna :request-items '(("Forum" . ((("PutRequest" . (("Item" . (("Name" . "Amazon DynamoDB")
                                                                                                 ("Category" . "Amazon Web Services"))))))
                                                                    (("PutRequest" . (("Item" . (("Name" . "Amazon RDS")
                                                                                                 ("Category" . "Amazon Web Services"))))))))
                                                        ("Thread" . ((("PutRequest" . (("Item" . (("ForumName" . "Amazon DynamoDB")
                                                                                                  ("Subject" . "Concurrent reads")))))))))
                                       :return-consumed-capacity "TOTAL")
        (build-json "\"RequestItems\":{\"Forum\":[{\"PutRequest\":{\"Item\":{\"Name\":{\"S\":\"Amazon DynamoDB\"},\"Category\":{\"S\":\"Amazon Web Services\"}}}},{\"PutRequest\":{\"Item\":{\"Name\":{\"S\":\"Amazon RDS\"},\"Category\":{\"S\":\"Amazon Web Services\"}}}}],\"Thread\":[{\"PutRequest\":{\"Item\":{\"ForumName\":{\"S\":\"Amazon DynamoDB\"},\"Subject\":{\"S\":\"Concurrent reads\"}}}}]}"
                    "\"ReturnConsumedCapacity\":\"TOTAL\"")
        "can return the correct JSON object."))

  (subtest "create-table"
    (is-error (create-table-content dyna :key-schema '((("AttributeName" . "ForumName") ("KeyType" . "HASH"))
                                                       (("AttributeName" . "Subject") ("KeyType" . "RANGE")))
                                         :attribute-definitions '((("AttributeName" . "ForumName") ("AttributeType" . "S"))
                                                                  (("AttributeName" . "Subject") ("AttributeType" . "S"))
                                                                  (("AttributeName" . "LastPostDateTime") ("AttributeType" . "S")))
                                         :local-secondary-indexes '((("IndexName" . "LastPostIndex")
                                                                     ("KeySchema" . ((("AttributeName" . "ForumName")
                                                                                      ("KeyType" . "HASH"))
                                                                                     (("AttributeName" . "LastPostDateTime")
                                                                                      ("KeyType" . "RANGE"))))
                                                                     ("Projection" . (("ProjectionType" . "KEYS_ONLY")))))
                                         :provisioned-throughput '(("ReadCapacityUnits" . 5)
                                                                   ("WriteCapacityUnits" . 5)))
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")
    (is-error (create-table-content dyna :table-name "Thread"
                                         :key-schema '((("AttributeName" . "ForumName") ("KeyType" . "HASH"))
                                                       (("AttributeName" . "Subject") ("KeyType" . "RANGE")))
                                         :local-secondary-indexes '((("IndexName" . "LastPostIndex")
                                                                     ("KeySchema" . ((("AttributeName" . "ForumName")
                                                                                      ("KeyType" . "HASH"))
                                                                                     (("AttributeName" . "LastPostDateTime")
                                                                                      ("KeyType" . "RANGE"))))
                                                                     ("Projection" . (("ProjectionType" . "KEYS_ONLY")))))
                                         :provisioned-throughput '(("ReadCapacityUnits" . 5)
                                                                   ("WriteCapacityUnits" . 5)))
              '<dyna-incomplete-argumet-error>
              "can raise the error without :attribute-definitions.")
    (is-error (create-table-content dyna :table-name "Thread"
                                         :attribute-definitions '((("AttributeName" . "ForumName") ("AttributeType" . "S"))
                                                                  (("AttributeName" . "Subject") ("AttributeType" . "S"))
                                                                  (("AttributeName" . "LastPostDateTime") ("AttributeType" . "S")))
                                         :local-secondary-indexes '((("IndexName" . "LastPostIndex")
                                                                     ("KeySchema" . ((("AttributeName" . "ForumName")
                                                                                      ("KeyType" . "HASH"))
                                                                                     (("AttributeName" . "LastPostDateTime")
                                                                                      ("KeyType" . "RANGE"))))
                                                                     ("Projection" . (("ProjectionType" . "KEYS_ONLY")))))
                                         :provisioned-throughput '(("ReadCapacityUnits" . 5)
                                                                   ("WriteCapacityUnits" . 5)))
              '<dyna-incomplete-argumet-error>
              "can raise the error without :key-schema.")
    (is-error (create-table-content dyna :table-name "Thread"
                                         :key-schema '((("AttributeName" . "ForumName") ("KeyType" . "HASH"))
                                                       (("AttributeName" . "Subject") ("KeyType" . "RANGE")))
                                         :attribute-definitions '((("AttributeName" . "ForumName") ("AttributeType" . "S"))
                                                                  (("AttributeName" . "Subject") ("AttributeType" . "S"))
                                                                  (("AttributeName" . "LastPostDateTime") ("AttributeType" . "S")))
                                         :local-secondary-indexes '((("IndexName" . "LastPostIndex")
                                                                     ("KeySchema" . ((("AttributeName" . "ForumName")
                                                                                      ("KeyType" . "HASH"))
                                                                                     (("AttributeName" . "LastPostDateTime")
                                                                                      ("KeyType" . "RANGE")))))
                                                                    ("Projection" . (("ProjectionType" . "KEYS_ONLY")))))
              '<dyna-incomplete-argumet-error>
              "can raise the error without :provisioned-throughput.")
    (is (create-table-content dyna :table-name "Thread"
                                   :key-schema '((("AttributeName" . "ForumName") ("KeyType" . "HASH"))
                                                 (("AttributeName" . "Subject") ("KeyType" . "RANGE")))
                                   :attribute-definitions '((("AttributeName" . "ForumName") ("AttributeType" . "S"))
                                                            (("AttributeName" . "Subject") ("AttributeType" . "S"))
                                                            (("AttributeName" . "LastPostDateTime") ("AttributeType" . "S")))
                                   :local-secondary-indexes '((("IndexName" . "LastPostIndex")
                                                               ("KeySchema" . ((("AttributeName" . "ForumName")
                                                                                ("KeyType" . "HASH"))
                                                                               (("AttributeName" . "LastPostDateTime")
                                                                                ("KeyType" . "RANGE"))))
                                                               ("Projection" . (("ProjectionType" . "KEYS_ONLY")))))
                                   :provisioned-throughput '(("ReadCapacityUnits" . 5)
                                                             ("WriteCapacityUnits" . 5)))
        (build-json "\"TableName\":\"Thread\""
                    "\"AttributeDefinitions\":[{\"AttributeName\":\"ForumName\",\"AttributeType\":\"S\"},{\"AttributeName\":\"Subject\",\"AttributeType\":\"S\"},{\"AttributeName\":\"LastPostDateTime\",\"AttributeType\":\"S\"}]"
                    "\"KeySchema\":[{\"AttributeName\":\"ForumName\",\"KeyType\":\"HASH\"},{\"AttributeName\":\"Subject\",\"KeyType\":\"RANGE\"}]"
                    "\"ProvisionedThroughput\":{\"ReadCapacityUnits\":5,\"WriteCapacityUnits\":5}"
                    "\"LocalSecondaryIndexes\":[{\"IndexName\":\"LastPostIndex\",\"KeySchema\":[{\"AttributeName\":\"ForumName\",\"KeyType\":\"HASH\"},{\"AttributeName\":\"LastPostDateTime\",\"KeyType\":\"RANGE\"}],\"Projection\":{\"ProjectionType\":\"KEYS_ONLY\"}}]")
        "can return the correct JSON object."))

  (subtest "delete-item"
    (is-error (delete-item-content dyna :key '(("ForumName" . "Amazon DynamoDB")))
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")
    (is-error (delete-item-content dyna :table-name "Thread")
              '<dyna-incomplete-argumet-error>
              "can raise the error without :key.")
    (is (delete-item-content dyna :table-name "Thread"
                                  :key '(("ForumName" . "Amazon DynamoDB"))
                                  :condition-expression "attribute_not_exists(Replies)"
                                  :return-values "ALL_OLD")
        (build-json "\"TableName\":\"Thread\""
                    "\"Key\":{\"ForumName\":{\"S\":\"Amazon DynamoDB\"}}"
                    "\"ReturnValues\":\"ALL_OLD\""
                    "\"ConditionExpression\":\"attribute_not_exists(Replies)\"")
        "can return the correct JSON object."))


  (subtest "delete-table"
    (is-error (delete-table-content dyna)
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")
    (is (delete-table-content dyna :table-name "Thread")
        "{\"TableName\":\"Thread\"}"
        "can return the correct JSON object."))

  (subtest "describe-table"
    (is-error (describe-table-content dyna)
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")
    (is (describe-table-content dyna :table-name "Thread")
        "{\"TableName\":\"Thread\"}"
        "can return the correct JSON object."))

  (subtest "get-item"
    (is-error (get-item-content dyna :key '(("Tags" . ("Multiple Items" "HelpMe"))))
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")
    (is-error (get-item-content dyna :table-name "Thread")
              '<dyna-incomplete-argumet-error>
              "can raise the error without :key.")
    (is (get-item-content dyna :table-name "Thread"
                               :key '(("Tags" . ("Multiple Items" "HelpMe")))
                               :consistent-read t
                               :return-consumed-capacity "TOTAL")
        (build-json "\"TableName\":\"Thread\""
                    "\"Key\":{\"Tags\":{\"SS\":[\"Multiple Items\",\"HelpMe\"]}}"
                    "\"ConsistentRead\":true"
                    "\"ReturnConsumedCapacity\":\"TOTAL\"")
        "can return the correct JSON object."))

  (subtest "list-tables"
    (is (list-tables-content dyna)
        "{}"
        "can return the correct JSON object."))

  (subtest "put-item"
    (is-error (put-item-content dyna :item '(("Tags" . ("Multiple Items" "HelpMe")) ("ForumName" . "Amazon DynamoDB")))
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")
    (is-error (put-item-content dyna :table-name "Thread")
              '<dyna-incomplete-argumet-error>
              "can raise the error without :key.")
    (is (put-item-content dyna :table-name "Thread"
                               :item '(("Tags" . ("Multiple Items" "HelpMe"))
                                       ("ForumName" . "Amazon DynamoDB"))
                               :condition-expression "ForumName <> :f and Subject <> :s"
                               :expression-attribute-values '((":f" . "Amazon DynamoDB")
                                                              (":s" . "Update multiple items")))
        (build-json "\"TableName\":\"Thread\""
                    "\"Item\":{\"Tags\":{\"SS\":[\"Multiple Items\",\"HelpMe\"]},\"ForumName\":{\"S\":\"Amazon DynamoDB\"}}"
                    "\"ConditionExpression\":\"ForumName <> :f and Subject <> :s\""
                    "\"ExpressionAttributeValues\":{\":f\":{\"S\":\"Amazon DynamoDB\"},\":s\":{\"S\":\"Update multiple items\"}}")
        "can return the correct JSON object."))

  (subtest "query"
    (is-error (query-content dyna :select "SPECIFIC_ATTRIBUTES"
                                  :attributes-to-get '("ForumName" "Subject")
                                  :index-name "LastPostIndex"
                                  :limit 3
                                  :consistent-read t
                                  :key-conditions '(("LastPostDateTime" . (("AttributeValueList" . ("20130101" "20130115"))
                                                                           ("ComparisonOperator" . "BETWEEN")))
                                                    ("ForumName" . (("AttributeValueList" . ("Amazon DynamoDB"))
                                                                    ("ComparisonOperator" . "EQ"))))
                                  :conditional-operator "OR"
                                  :query-filter '(("Subject" . (("AttributeValueList" . ("Update multiple items" "Concurrent reads"))
                                                                ("ComparisonOperator" . "EQ"))))
                                  :exclusive-start-key '(("ForumName" . "Amazon RDS"))
                                  :filter-expression "#r > :num"
                                  :expression-attribute-names '(("#r" . "Replies"))
                                  :expression-attribute-values '((":num" . 1))
                                  :scan-index-forward t
                                  :return-consumed-capacity "TOTAL")
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")
    (is-error (query-content dyna :table-name "Thread"
                                  :select "SPECIFIC_ATTRIBUTES"
                                  :attributes-to-get '("ForumName" "Subject")
                                  :index-name "LastPostIndex"
                                  :limit 3
                                  :consistent-read t
                                  :conditional-operator "OR"
                                  :query-filter '(("Subject" . (("AttributeValueList" . ("Update multiple items" "Concurrent reads"))
                                                                ("ComparisonOperator" . "EQ"))))
                                  :exclusive-start-key '(("ForumName" . "Amazon RDS"))
                                  :filter-expression "#r > :num"
                                  :expression-attribute-names '(("#r" . "Replies"))
                                  :expression-attribute-values '((":num" . 1))
                                  :scan-index-forward t
                                  :return-consumed-capacity "TOTAL")
              '<dyna-incomplete-argumet-error>
              "can raise the error without :key-conditions.")
    (is (query-content dyna :table-name "Thread"
                            :select "SPECIFIC_ATTRIBUTES"
                            :attributes-to-get '("ForumName" "Subject")
                            :index-name "LastPostIndex"
                            :limit 3
                            :consistent-read t
                            :key-conditions '(("LastPostDateTime" . (("AttributeValueList" . ("20130101" "20130115"))
                                                                     ("ComparisonOperator" . "BETWEEN")))
                                              ("ForumName" . (("AttributeValueList" . ("Amazon DynamoDB"))
                                                              ("ComparisonOperator" . "EQ"))))
                            :conditional-operator "OR"
                            :query-filter '(("Subject" . (("AttributeValueList" . ("Update multiple items" "Concurrent reads"))
                                                          ("ComparisonOperator" . "EQ"))))
                            :exclusive-start-key '(("ForumName" . "Amazon RDS"))
                            :filter-expression "#r > :num"
                            :expression-attribute-names '(("#r" . "Replies"))
                            :expression-attribute-values '((":num" . 1))
                            :scan-index-forward t
                            :return-consumed-capacity "TOTAL")
        (build-json "\"TableName\":\"Thread\""
                    "\"KeyConditions\":{\"LastPostDateTime\":{\"AttributeValueList\":[{\"S\":\"20130101\"},{\"S\":\"20130115\"}],\"ComparisonOperator\":\"BETWEEN\"},\"ForumName\":{\"AttributeValueList\":[{\"S\":\"Amazon DynamoDB\"}],\"ComparisonOperator\":\"EQ\"}}"
                    "\"ReturnConsumedCapacity\":\"TOTAL\""
                    "\"AttributesToGet\":[\"ForumName\",\"Subject\"]"
                    "\"IndexName\":\"LastPostIndex\""
                    "\"Select\":\"SPECIFIC_ATTRIBUTES\""
                    "\"Limit\":3"
                    "\"ConsistentRead\":true"
                    "\"ConditionalOperator\":\"OR\""
                    "\"ExclusiveStartKey\":{\"ForumName\":{\"S\":\"Amazon RDS\"}}"
                    "\"ExpressionAttributeValues\":{\":num\":{\"N\":\"1\"}}"
                    "\"ExpressionAttributeNames\":{\"#r\":\"Replies\"}"
                    "\"FilterExpression\":\"#r > :num\""
                    "\"QueryFilter\":{\"Subject\":{\"AttributeValueList\":[{\"S\":\"Update multiple items\"}"
                    "{\"S\":\"Concurrent reads\"}]"
                    "\"ComparisonOperator\":\"EQ\"}}"
                    "\"ScanIndexForward\":true")
        "can return the correct JSON object."))

  (subtest "scan"
    (is-error (scan-content dyna :select "SPECIFIC_ATTRIBUTES"
                                 :attributes-to-get '("ForumName" "Subject")
                                 :index-name "LastPostIndex"
                                 :limit 3
                                 :consistent-read t
                                 :conditional-operator "OR"
                                 :scan-filter '(("Subject" . (("AttributeValueList" . ("Update multiple items" "Concurrent reads"))
                                                              ("ComparisonOperator" . "EQ"))))
                                 :exclusive-start-key '(("ForumName" . "Amazon RDS"))
                                 :filter-expression "#r > :num"
                                 :expression-attribute-names '(("#r" . "Replies"))
                                 :expression-attribute-values '((":num" . 1))
                                 :scan-index-forward t
                                 :return-consumed-capacity "TOTAL"
                                 :segment 1
                                 :total-segments 4)
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")

    (is (scan-content dyna :table-name "Thread"
                           :select "SPECIFIC_ATTRIBUTES"
                           :attributes-to-get '("ForumName" "Subject")
                           :index-name "LastPostIndex"
                           :limit 3
                           :consistent-read t
                           :conditional-operator "OR"
                           :scan-filter '(("Subject" . (("AttributeValueList" . ("Update multiple items" "Concurrent reads"))
                                                        ("ComparisonOperator" . "EQ"))))
                           :exclusive-start-key '(("ForumName" . "Amazon RDS"))
                           :filter-expression "#r > :num"
                           :expression-attribute-names '(("#r" . "Replies"))
                           :expression-attribute-values '((":num" . 1))
                           :scan-index-forward t
                           :return-consumed-capacity "TOTAL"
                           :segment 1
                           :total-segments 4)
        (build-json "\"TableName\":\"Thread\""
                    "\"ReturnConsumedCapacity\":\"TOTAL\""
                    "\"AttributesToGet\":[\"ForumName\",\"Subject\"]"
                    "\"IndexName\":\"LastPostIndex\""
                    "\"Select\":\"SPECIFIC_ATTRIBUTES\""
                    "\"Limit\":3"
                    "\"ConsistentRead\":true"
                    "\"ConditionalOperator\":\"OR\""
                    "\"ExclusiveStartKey\":{\"ForumName\":{\"S\":\"Amazon RDS\"}}"
                    "\"ExpressionAttributeValues\":{\":num\":{\"N\":\"1\"}}"
                    "\"ExpressionAttributeNames\":{\"#r\":\"Replies\"}"
                    "\"FilterExpression\":\"#r > :num\""
                    "\"ScanFilter\":{\"Subject\":{\"AttributeValueList\":[{\"S\":\"Update multiple items\"}"
                    "{\"S\":\"Concurrent reads\"}]"
                    "\"ComparisonOperator\":\"EQ\"}}"
                    "\"ScanIndexForward\":true"
                    "\"Segment\":1"
                    "\"TotalSegments\":4")
        "can return the correct JSON object."))

  (subtest "update-item"
    (is-error (update-item-content dyna :key '(("ForumName" . "Amazon DynamoDB"))
                                        :update-expression "set Replies = Replies + :num"
                                        :expression-attribute-values '((":num" . 1))
                                        :return-values "NONE")
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")
    (is-error (update-item-content dyna :table-name "Thread"
                                        :update-expression "set Replies = Replies + :num"
                                        :expression-attribute-values '((":num" . 1))
                                        :return-values "NONE")
              '<dyna-incomplete-argumet-error>
              "can raise the error without :key.")
    (is (update-item-content dyna :table-name "Thread"
                                  :key '(("ForumName" . "Amazon DynamoDB"))
                                  :update-expression "set Replies = Replies + :num"
                                  :expression-attribute-values '((":num" . 1))
                                  :return-values "NONE")
        (build-json "\"TableName\":\"Thread\""
                    "\"Key\":{\"ForumName\":{\"S\":\"Amazon DynamoDB\"}}"
                    "\"UpdateExpression\":\"set Replies = Replies + :num\""
                    "\"ReturnValues\":\"NONE\""
                    "\"ExpressionAttributeValues\":{\":num\":{\"N\":\"1\"}}")
        "can return the correct JSON object."))

  (subtest "update-table"
    (is-error (update-table-content dyna :attribute-definitions '((("AttributeName" . "ForumName") ("AttributeType" . "S"))
                                                                  (("AttributeName" . "Subject") ("AttributeType" . "S"))
                                                                  (("AttributeName" . "LastPostDateTime") ("AttributeType" . "S")))
                                         :provisioned-throughput '(("ReadCapacityUnits" . 5)
                                                                   ("WriteCapacityUnits" . 5)))
              '<dyna-incomplete-argumet-error>
              "can raise the error without :table-name.")
    (is (update-table-content dyna :table-name "Thread"
                                   :attribute-definitions '((("AttributeName" . "ForumName") ("AttributeType" . "S"))
                                                            (("AttributeName" . "Subject") ("AttributeType" . "S"))
                                                            (("AttributeName" . "LastPostDateTime") ("AttributeType" . "S")))
                                   :provisioned-throughput '(("ReadCapacityUnits" . 5)
                                                             ("WriteCapacityUnits" . 5)))
        (build-json
         "\"TableName\":\"Thread\""
         "\"AttributeDefinitions\":[{\"AttributeName\":\"ForumName\",\"AttributeType\":\"S\"},{\"AttributeName\":\"Subject\",\"AttributeType\":\"S\"},{\"AttributeName\":\"LastPostDateTime\",\"AttributeType\":\"S\"}]"
         "\"ProvisionedThroughput\":{\"ReadCapacityUnits\":5,\"WriteCapacityUnits\":5}")
        "can return the correct JSON object.")))

(finalize)
