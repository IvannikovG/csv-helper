(ns csv-helper.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io  :as io]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn remove-spaces
  [header]
  (str/trim header))

(defn replace-special-symbols
  [header symbols]
  (when header
    (str/replace header symbols "_")))

(defn remove-symbols-around
  [string]
  (cond (and (str/ends-with? string "_")
             (str/starts-with? string "_")) (remove-symbols-around
                                             (->> string
                                                  drop-last
                                                  (drop 1)
                                                  (apply str)))
        (str/starts-with? string "_")       (remove-symbols-around
                                             (->> string
                                                  (drop 1)
                                                  (apply str)))
        (str/ends-with? string "_")         (remove-symbols-around
                                             (->> string
                                                  drop-last
                                                  (apply str)))
        :else string))

(defn clean-header
  "Clean a header here"
  [header {:keys [special-symbols] :as _config}]
  (-> header
      remove-spaces
      (replace-special-symbols (or special-symbols #"[\$ %,-/\.\\@*]"))
      str/lower-case
      remove-symbols-around
      keyword))

(defn clean-headers
  [headers config]
  (map (fn [header] (clean-header header config)) headers))

(defn read-csv
  [path-to-file]
  (with-open [reader (io/reader path-to-file)]
    (doall
     (csv/read-csv reader))))

(defn add-line-meta
  [acc line]
  (-> line
      (assoc :w_line_number (:count acc))))

(defn transform-line
  [_acc line]
  line)

(defn arrays->ndjson
  [arrays config]
  (assert (sequential? arrays))
  (let [headers (-> arrays first (clean-headers config))
        lines   (rest arrays)]
    (:lines (reduce (fn [acc line]
                      (let [count (:count acc)
                            lines (:lines acc)]
                        {:count (inc count)
                         :lines (conj lines (->> line
                                                 (map vector headers)
                                                 (into {})
                                                 (transform-line acc)
                                                 (add-line-meta acc)))}))
                    {:count 1
                     :lines []}
                   lines))))

(defn select-keys-from-line
  [line keys]
  (select-keys line keys))

(defn select-keys-from-lines
  [lines keys]
  (if (empty? keys)
    lines
    (map (fn [line] (select-keys-from-line line keys)) lines)))

(defn remove-keys-from-line
  [line keys]
  (apply dissoc line keys))

(defn remove-keys-from-lines
  [lines keys]
  (let [default-keys [:w_line_number]]
    (if (empty? keys)
      (map (fn [line] (remove-keys-from-line line default-keys)) lines)
      (map (fn [line] (remove-keys-from-line line keys)) lines))))

(defn select-lines-from-to
  "to:  is int
  from: is int
  -> indexes from - to"
  [lines from to]
  (let [safe-from (or from 1)
        safe-to   (or to (count lines))]
    (filter (fn [line]
              (let [line-number (:w_line_number line)]
                (and (>= line-number safe-from)
                     (<= line-number safe-to)))) lines)))

(defn select-lines-by-index
  [lines index-array]
  (if (empty? index-array)
    lines
    (filter (fn [line]
              (let [line-number (:w_line_number line)]
                (contains? (set index-array) line-number))) lines)))

(defn ndjson->arrays
  [ndjson]
  (let [headers (->> ndjson first keys (map name))]
    (reduce (fn [acc line]
              (let [line-values (-> line vals)]
                (conj acc line-values))) [headers] ndjson)))

(defn write-csv
  "Lines must be [[] [] []]"
  [file-path lines]
  (with-open [writer (io/writer file-path)]
    (csv/write-csv writer lines)))

(defn format-csv
  "
  config is a hashmap
  parameters:
  keys-to-get    [:a :b :c], default to all lines
  keys-to-remove [:a :b :c], defaults to [:w_line_number]
  special-symbols regexp, defaults to \"[$ %,-/.@*]\" <- these symbols will be removed from the headers
  from: 1, default to 1
  to: 1, default to last line
  line-idx [1 2 5], default to all lines "
  [file-path {:keys [keys-to-get keys-to-remove from to line-idx] :as config}]
  (let [csv-arrays       (read-csv file-path)
        csv-ndjson       (arrays->ndjson csv-arrays config)
        csv-slice        (select-lines-from-to csv-ndjson from to)
        csv-slice-idx    (select-lines-by-index csv-slice line-idx)
        csv-with-keys    (select-keys-from-lines csv-slice-idx keys-to-get)
        csv-without-keys (remove-keys-from-lines csv-with-keys keys-to-remove)]
    csv-without-keys))


(defn format-and-save-csv
  [file-path path-to-save config]
  (let [formatted-csv (-> file-path
                          (format-csv config)
                          ndjson->arrays)]
    (write-csv path-to-save formatted-csv)))

(defn print-csv
  [file-path]
  (let [csv (read-csv file-path)]
    (pprint/pprint csv)))

(defn print-nd
  [file-path]
  (let [csv (arrays->ndjson (read-csv file-path) {:keys-to-remove [:default]})]
    (pprint/pprint csv)))


(comment

  (print-nd "test/document_data/csv/test.csv")

  (def test (format-csv "test/document_data/csv/test.csv"
                        {:keys-to-get [:test_field1 :w_line_number :key]
                         :keys-to-remove [:key]
                         :line-idx [1 2 3]
                         :from 2
                         :to 3}))

  (format-and-save-csv "test/document_data/csv/test.csv"
                       "test/document_data/csv/formatted_test.csv"
                       {:keys-to-get [:test_field1 :w_line_number :key]
                        :keys-to-remove [:key]
                        :line-idx [1 2 3]
                        :from 2
                        :to 3} )

  )
