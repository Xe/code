(ns constellation.atheme
  (:require [necessary-evil.core :as xml-rpc]))

(defn anon-command [rpcpath service command args]
  (xml-rpc/call rpcpath :atheme.command "*" "*" "*" service command args))

