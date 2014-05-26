(defn merge_ [list1 list2]
  (defn private_merge [large small ratio results]
    (if (or (seq large) (seq small))
      (recur
        (drop ratio large) (drop 1 small) ratio
        (concat results (take ratio large) (take 1 small)))
      results))
  (let [ratio (int (/ (count list1) (count list2)))]
    (if (<= ratio 0)
      (recur list2 list1)
      (if (= (mod (count list1) (count list2)) 0)
        (private_merge list1 list2 (dec ratio) [])
        (private_merge list1 list2 ratio [])))))

(defn test-merge_ []
  (println (merge_ [1 2 3 4] [\a \b \c]))
  (println (merge_ [1 2 3 4 5] [\a \b]))
  (println (merge_ [\a \b \c] [1 2 3 4 5]))
  (println (merge_ [1 2 3 4 5 6] [\a \b]))
  (println (merge_ [1 2 3 4 5 6 7 8] [\a \b \c \d \e])))
(test-merge_)
