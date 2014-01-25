(ns comic-site.server
  (:require [noir.server :as server]))

(server/load-views "src/comic_site/views/")

(defn -main [& m]
  (println "Starting up")
  (let [mode (keyword (or (first m) :dev))
        port (Integer. (get (System/getenv) "PORT" "8080"))]
    (server/start port {:mode mode
                        :ns 'comic-site})))

