(ns fruit-exchange.order-matcher
  (:use
     conduit.core
     conduit.rabbitmq))

;; An order is a hash-map with keys:
;;    :account-number (of buyer or seller)
;;    :type (:buy or :sell)
;;    :size (integer > 0)
;;    :price (dollars and cents)

;; The order book is a hash-map with keys:
;;    :bids (a list of orders to buy)
;;    :offers (a list of orders to sell)

;; A transaction is a hash map with keys:
;;    :buyer (account number of buyer)
;;    :seller (account number of seller)
;;    :price
;;    :size

(def buy-order? #(= :buy (:type %)))
(def sell-order? #(= :sell (:type %)))

(defn order-sort [a b]
  (let [gt-or-lt (if (buy-order? a)
                   >
                   <)]
    (gt-or-lt (:price a) (:price b))))

(defn conj-order [order-list new-order]
  (if (> (get new-order :size 0) 0)
    (sort (comparator order-sort)
          (conj order-list new-order))
    order-list))

(defn add-order [order-book new-order]
  (let [order-side (if (buy-order? new-order)
                     :bids
                     :offers)]
    (update-in order-book [order-side]
               conj-order 
               new-order)))

(defn crossed-order [[order-book new-order]]
  (when (> (:size new-order) 0)
    (if (buy-order? new-order)
      (and (seq (:offers order-book))
           (>= (:price new-order)
               (:price (first (:offers order-book)))))
      (and (seq (:bids order-book))
           (<= (:price new-order) (:price (first (:bids order-book))))))))

(defn new-transaction [buy-side sell-side]
  {:size (min (:size buy-side)
              (:size sell-side))
   :buyer (:account-number buy-side)
   :seller (:account-number sell-side)
   :price (/ (+ (:price sell-side)
                (:price buy-side))
             2)})

(defn remove-order-size [order-list size]
  (conj (rest order-list)
        (update-in (first order-list) [:size] - size)))

(defn remove-order [order-book new-order]
  (let [buying (buy-order? new-order)]
    (cond
      (and buying
           (>= (:size new-order) (:size (first (:offers order-book)))))
      (update-in order-book [:offers] rest)

      buying 
      (update-in order-book [:offers] 
                 remove-order-size
                 (:size new-order))

      (>= (:size new-order) (:size (first (:bids order-book))))
      (update-in order-book [:bids] rest)
      
      :else
      (update-in order-book [:bids] 
                 remove-order-size
                 (:size new-order)))))

(defn create-transaction [[order-book new-order]]
  (let [buying (buy-order? new-order)
        buy-side (if buying
                   new-order
                   (first (:bids order-book)))
        sell-side (if buying
                   (first (:offers order-book))
                    new-order)
        trans (new-transaction buy-side sell-side)
        new-book (remove-order order-book new-order)]
    [(update-in new-book [:transactions] conj trans)
     (update-in new-order [:size] - (:size trans))]))

(defn match-order [order-pair]
  (if (crossed-order order-pair)
    (recur (create-transaction order-pair))
    (apply add-order order-pair)))

