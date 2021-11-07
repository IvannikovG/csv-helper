(ns csv-helper.core-test
  (:require [csv-helper.core :as sut]
            [matcho.core :as matcho]
            [clojure.test :as t :refer [deftest is]]))

(deftest core-test
  (sut/format-and-save-csv "test/document_data/csv/test.csv"
                           "test/document_data/csv/formatted_test.csv"
                           {:keys-to-get [:test_field1 :w_line_number :key :line_num]
                            :keys-to-remove [:key]
                            :line-idx [1 2 3]
                            :from 2
                            :to 3} )

  (matcho/match
   (sut/format-csv "test/document_data/csv/formatted_test.csv" {})
   [{:test_field1 " 4", :line_num "2"} {:test_field1 " 8", :line_num "3"}]))
