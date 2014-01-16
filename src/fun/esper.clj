(ns example.core
  (:use [clj-esper.core]))

;; Events 
(defevent OrderEvent [inst :string, price :double])

;; Statements
(defstatement avg-price "SELECT avg(price) FROM OrderEvent.win:time(30 sec)")
(defstatement below-avg "SELECT avg(price) FROM OrderEvent.win:time(30 sec) having avg(price) < 50")

;; Handlers
(defn handler1
  [msg]
  (println "avg: " (.get msg "avg(price)")))

(defn handler2
  [msg]
  (println "Triggered -- Lets go SHORT!"))

;; Inits Esper with one event and two statements (from above)
(defn init []
  (with-esper service {:uri "/some-uri" :events #{OrderEvent}}
    (attach-statement avg-price handler1)
    (attach-statement below-avg handler2)))

;; Testing
(defn test-send [price]
  (with-esper service {:uri "/some-uri"}
    (trigger-event (new-event OrderEvent :inst "MSFT" :price price))))

(init)
(test-send 100.00)
(test-send (* (rand) 50))