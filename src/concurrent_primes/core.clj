(ns concurrent-primes.core
  (:require [clojure.core.async :as async :refer [chan go <! >!]])
  (:gen-class :main true))

;;; concurrent prime sieve in Clojure using core.async
;; inspired by a similar implementation in Go
;; http://golang.org/doc/play/sieve.go
 
(defmacro go-forever
  "An infinite loop that runs in a go block."
  [& body]
  `(go (loop []
         ~@body
         (recur))))
 
 
(defmacro go-loop
  "Like clojure.core/loop but runs in a go block."
  [bindings & body]
  `(go (loop ~bindings
         ~@body)))
 
 
(defn filter-chan
  "Somewhat like clojure.core/filter, but operates on a
   channel and returns a new channel. "
  [pred ch]
  (let [out (chan)]
    (go-forever
     (let [val (<! ch)]
       (when (pred val)
         (>! out val))))
    out))
 
 
(defn generate
  "Generate all numbers into channel."
  [ch]
  (go-loop [n 2]
    (>! ch n)
    (recur (inc n))))
 
 
(defn sieve
  "Generate n primes via a concurrent prime sieve.
   Successive prime filtering is done by daisy-chaining channels."
  [n]
  (let [in (chan)]
    (generate in)
    (go-loop [i 0 ch in]
      (when (< i n)
        (let [prime (<! ch)
              nxt (filter-chan #(pos? (rem % prime)) ch)]
          (println prime)
          (recur (inc i) nxt))))
    :ok))

;; (sieve 20)

(defn non-concurrent-sieve
  ;; http://clojuredocs.org/clojure_core/clojure.core/lazy-seq#example_1000
  ;; for reference
  [s]
  (cons (first s)
        (lazy-seq (non-concurrent-sieve
                   (filter #(pos? (rem % (first s))) (rest s))
                                        ))))

;; (take 20 (non-concurrent-sieve (iterate inc 2)))


(defn my-generate
  []
  (iterate inc 2))
  
(defn my-sieve
  [n]
  (let [in (my-generate)]
    (loop [i 0 ch in]
      (when (< i n)
        (let [prime (first ch)
              nxt (filter #(pos? (rem % prime)) ch)]
          (println prime)
          (recur (inc i) nxt))))
    :ok))


(defn -main [& args]
  (prn (format "args=%s" args))
  (if (not (empty? args))
    (sieve (first args))))
    ;; (do
    ;;   (prn "blik")
    ;;   (sieve (first args)))))

