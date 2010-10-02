(ns fruit-exchange.core
  (:use
     conduit.core
     conduit.rabbitmq
     fruit-exchange.order-matcher)
  (:import
     [com.rabbitmq.client ConnectionParameters ConnectionFactory]))

(def record-transaction
  (a-rabbitmq
    "transactions"
    "record-trans"
    (a-arr println)))

(def order-matcher
  (a-loop (a-arr match-order)
          {}
          (a-arr #(dissoc % :transactions))))

(defn market [fruit]
  (a-rabbitmq
    (name fruit)
    "match-order"
    (a-comp
      order-matcher
      (a-arr :transactions)
      (disperse (a-arr #(assoc % :product fruit)))
      (disperse record-transaction))))

(def fruit-exchange
  (a-rabbitmq
    "orders"
    "exchange"
    (a-comp
      (a-all (a-arr :product)
             pass-through)
      (a-select 
        :apple (market :apple)
        :blueberry (market :blueberry)
        :cherry (market :cherry)))))

(defn rabbitmq-connection [host vhost user password]
  ;; for rabbitmq client 1.7.2
  (let [params (doto (ConnectionParameters.)
                 (.setVirtualHost vhost)
                 (.setUsername "guest")
                 (.setPassword "guest"))
        factory (ConnectionFactory. params)]
    (.newConnection factory "localhost")))

(defn exchange-thread [queue]
  (fn []
    (with-open [connection (rabbitmq-connection "localhost" "/" "guest" "guest")
                channel (.createChannel connection)]
      (.exchangeDeclare channel "fruit-bot" "direct")
      (rabbitmq-run fruit-exchange queue channel "fruit-bot"))))
  
(defn start-exchange-thread [queue]
    (doto (new Thread (exchange-thread queue))
      (.start)))

(start-exchange-thread "orders")
(start-exchange-thread "apple")
(start-exchange-thread "blueberry")
(start-exchange-thread "cherry")
(start-exchange-thread "transactions")

(with-open [connection (rabbitmq-connection "localhost" "/" "guest" "guest")
            channel (.createChannel connection)]
  (conduit-rabbitmq channel "fruit-bot"
                    (conduit-map fruit-exchange
                                 [{:product :apple :type :buy :price 5 :size 1 :account-number "buyer"}
                                  {:product :apple :type :sell :price 5 :size 1 :account-number "seller"}
                                  {:product :blueberry :type :buy :price 5 :size 1 :account-number "buyer"}
                                  {:product :blueberry :type :sell :price 5 :size 1 :account-number "seller"}
                                  {:product :cherry :type :buy :price 5 :size 1 :account-number "buyer"}
                                  {:product :cherry :type :sell :price 5 :size 1 :account-number "seller"}])))
