(ns constellation.server
  (:require [noir.server :as server] [necessary-evil.core :as xml-rpc]))

(server/load-views-ns 'constellation.views)

(defn -main [& m]
  (let [mode (keyword (or (first m) :dev))
        port (Integer. (get (System/getenv) "PORT" "8080"))]
    (server/start port {:mode mode
                        :ns 'constellation})))

