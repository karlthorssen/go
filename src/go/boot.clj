(ns go.boot
  (:require [overtone.core :refer :all :exclude [tap]]))

(comment
  (server-status)
  (kill-server)
  (boot-internal-server)
  (reset! connected :no)

  @osample/loaded-samples*



  (require '[overtone.sc.machinery.server.connection :as connection])
  @connection/connection-status*
  (connection/shutdown-server)
  )

(reset! overtone.samples.freesound/*access-token* "AQ160WVqkwjkAEbkPRU3Mwe4p0bhZE")

(defonce connected (atom :no))
(when-not (= @connected :happy-hacking)
  (reset! connected (connect-external-server 57110)))
