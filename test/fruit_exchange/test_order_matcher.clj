(ns fruit-exchange.test-order-matcher
  (:use fruit-exchange.order-matcher :reload-all)
  (:use
     clojure.test
     clojure.pprint))

(deftest test-order-sort
         (is (= [{:type :buy :price 10}
                 {:type :buy :price 5}
                 {:type :buy :price 3}
                 {:type :buy :price 3}]
                (sort (comparator order-sort)
                      [{:type :buy :price 5}
                       {:type :buy :price 3}
                       {:type :buy :price 10}
                       {:type :buy :price 3}])))

         (is (= [{:type :sell :price 3}
                 {:type :sell :price 3}
                 {:type :sell :price 5}
                 {:type :sell :price 10}]
                (sort (comparator order-sort)
                      [{:type :sell :price 5}
                       {:type :sell :price 3}
                       {:type :sell :price 10}
                       {:type :sell :price 3}]))))

(deftest test-add-order
         (is (= {:bids [{:type :buy :price 10 :size 1}
                        {:type :buy :price 5 :size 1}
                        {:type :buy :price 3 :size 1}
                        {:type :buy :price 3 :size 1}]}
                (reduce add-order
                        {}
                        [{:type :buy :price 5 :size 1}
                         {:type :buy :price 3 :size 1}
                         {:type :buy :price 10 :size 0}
                         {:type :buy :price 10 :size 1}
                         {:type :buy :price 3 :size 1}])))

         (is (= {:offers [{:type :sell :price 3 :size 1}
                          {:type :sell :price 3 :size 1}
                          {:type :sell :price 5 :size 1}
                          {:type :sell :price 10 :size 1}]}
                (reduce add-order
                        {}
                        [{:type :sell :price 5 :size 1}
                         {:type :sell :price 3 :size 1}
                         {:type :sell :price 7 :size -1}
                         {:type :sell :price 10 :size 1}
                         {:type :sell :price 3 :size 1}])))
         
         (is (= {:bids [{:type :buy :price 10 :size 1}
                        {:type :buy :price 5 :size 1}
                        {:type :buy :price 3 :size 1}
                        {:type :buy :price 3 :size 1}]
                 :offers [{:type :sell :price 3 :size 1}
                          {:type :sell :price 3 :size 1}
                          {:type :sell :price 5 :size 1}
                          {:type :sell :price 10 :size 1}]}
                (reduce add-order
                        {}
                        [{:type :sell :price 5 :size 1}
                         {:type :buy :price 5 :size 1}
                         {:type :buy :price 10 :size 1}
                         {:type :buy :price 3 :size 1}
                         {:type :sell :price 3 :size 1}
                         {:type :sell :price 7 :size -1}
                         {:type :sell :price 10 :size 1}
                         {:type :buy :price 3 :size 1}
                         {:type :buy :price 10 :size 0}
                         {:type :sell :price 3 :size 1}]))))

(deftest test-crossed-order
         (is (not (crossed-order [{:offers []}
                                  {:type :buy
                                   :size 1
                                   :price 10}])))
         (is (not (crossed-order [{} {:type :buy
                                      :size 1
                                      :price 10}])))
         (is (crossed-order [{:offers [{:price 9}]}
                             {:type :buy
                              :size 1
                              :price 10}]))
         (is (not (crossed-order [{} {:type :sell
                                      :size 1
                                      :price 10}])))
         (is (crossed-order [{:bids [{:price 11}]}
                             {:type :sell
                              :size 1
                              :price 10}]))
         (is (crossed-order [{:bids [{:type :buy :price 10 :size 1
                                      :account-number "buyer"}
                                     {:type :buy :price 3 :size 1
                                      :account-number "buyer"}]}
                             {:type :sell :price 3 :size 1
                              :account-number "seller"}]))
         (is (not (crossed-order [{:bids [{:type :buy :price 3 :size 1
                                           :account-number "buyer"}]}
                                  {:type :sell :price 3 :size 0
                                   :account-number "seller"}]))))

(deftest test-new-transaction
         (is (= {:size 5
                 :buyer "buyer"
                 :seller "seller"
                 :price 10.5}
                (new-transaction {:size 5
                                  :price 11
                                  :account-number "buyer"}
                                 {:size 10
                                  :price 10
                                  :account-number "seller"}))))

(deftest test-remove-order-size
         (is (= [{:size 4}]
                (remove-order-size [{:size 11}] 7))))

(deftest test-remove-order
         (is (= {:offers [{:size 4}]}
                (remove-order {:offers [{:size 11}
                                        {:size 4}]}
                              {:type :buy
                               :size 20})))
         (is (= {:offers [{:size 4}]}
                (remove-order {:offers [{:size 11}]}
                              {:type :buy
                               :size 7})))
         (is (= {:bids [{:size 10}]}
                (remove-order {:bids [{:size 5}
                                      {:size 10}]}
                              {:type :sell
                               :size 6})))
         (is (= {:bids [{:size 19}]}
                (remove-order {:bids [{:size 25}]}
                              {:type :sell
                               :size 6}))))

(deftest test-create-transaction
         (is (= [{:offers []
                  :transactions [{:size 3
                                  :buyer "buyer"
                                  :seller "seller"
                                  :price 10.5}]}
                 {:size 0
                  :type :buy
                  :account-number "buyer"
                  :price 11}]
                (create-transaction [{:offers [{:size 3
                                                :account-number "seller"
                                                :price 10}]}
                                     {:type :buy
                                      :size 3
                                      :account-number "buyer"
                                      :price 11}])))
         (is (= [{:bids []
                  :transactions [{:size 3
                                  :buyer "buyer"
                                  :seller "seller"
                                  :price 10.5}]}
                 {:size 0
                  :type :sell
                  :account-number "seller"
                  :price 10}]
                (create-transaction [{:bids [{:size 3
                                              :account-number "buyer"
                                              :price 11}]}
                                     {:type :sell
                                      :size 3
                                      :account-number "seller"
                                      :price 10}]))))

(deftest test-match-order
         (is (= {:bids
                 [{:type :buy :price 15 :size 1 :account-number "big-spender"}
                  {:type :buy :price 3 :size 1 :account-number "buyer"}
                  {:type :buy :price 3 :size 1 :account-number "buyer"}]
                 :transactions
                 [{:size 1 :buyer "big-spender" :seller "seller" :price 9}
                  {:size 1 :buyer "big-spender" :seller "seller" :price 25/2}
                  {:size 1 :buyer "buyer" :seller "seller" :price 13/2}
                  {:size 1 :buyer "buyer" :seller "seller" :price 5}]
                 :offers []}
                (reduce (comp match-order vector)
                        {}
                        [{:type :sell :price 5 :size 1 :account-number "seller"}
                         {:type :buy :price 5 :size 1 :account-number "buyer"}
                         {:type :buy :price 10 :size 1 :account-number "buyer"}
                         {:type :buy :price 3 :size 1 :account-number "buyer"}
                         {:type :sell :price 3 :size 1 :account-number "seller"}
                         {:type :sell :price 7 :size -1 :account-number "seller"}
                         {:type :sell :price 10 :size 1 :account-number "seller"}
                         {:type :buy :price 3 :size 1 :account-number "buyer"}
                         {:type :buy :price 10 :size 0 :account-number "buyer"}
                         {:type :buy :price 15 :size 3 :account-number "big-spender"}
                         {:type :sell :price 3 :size 1 :account-number "seller"}]))))

(run-tests)
nil

