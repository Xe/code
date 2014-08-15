(ns QueueingAssignment.core)

(deftype Job [name time])

(def jobq clojure.lang.PersistentQueue/EMPTY)

(defn get-input [prompt]
    (println prompt)
    (read-line))

(defn factorial [x]
  (if (zero? x) 1 
    (* x (factorial (- x 1)))))
