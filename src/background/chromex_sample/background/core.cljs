(ns chromex-sample.background.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [goog.string :as gstring]
            [goog.string.format]
            [goog.object]
            [clojure.string :as string]
            [cljs.core.async :refer [<! chan]]
            [chromex.logging :refer-macros [log info warn error group group-end]]
            [chromex.chrome-event-channel :refer [make-chrome-event-channel]]
            [chromex.protocols.chrome-port :refer [post-message! get-sender]]
            [chromex.ext.tabs :as tabs]
            [chromex.ext.runtime :as runtime]
            [chromex.ext.browser-action :as browser-action]
            [chromex-sample.background.storage :refer [test-storage!]]))

(def clients (atom []))

; -- clients manipulation ---------------------------------------------------------------------------------------------------

(defn add-client! [client]
  (log "BACKGROUND: client connected" (get-sender client))
  (swap! clients conj client))

(defn remove-client! [client]
  (log "BACKGROUND: client disconnected" (get-sender client))
  (let [remove-item (fn [coll item] (remove #(identical? item %) coll))]
    (swap! clients remove-item client)))

; -- client event loop ------------------------------------------------------------------------------------------------------

(defn run-client-message-loop! [client]
  (log "BACKGROUND: starting event loop for client:" (get-sender client))
  (go-loop []
           (when-some [message (<! client)]
             (log "BACKGROUND: got client message:" message "from" (get-sender client))
             (recur))
           (log "BACKGROUND: leaving event loop for client:" (get-sender client))
           (remove-client! client)))

; -- event handlers ---------------------------------------------------------------------------------------------------------

(defn handle-client-connection! [client]
  (add-client! client)
  (post-message! client "hello from BACKGROUND PAGE!")
  (run-client-message-loop! client))

(defn tell-clients-about-new-tab! []
  (doseq [client @clients]
    (post-message! client "a new tab was created")))

(def youtube-regex (re-pattern "(?:youtube\\.com/\\S*(?:(?:\\/e(?:mbed))?/|watch\\?(?:\\S*?&?v\\=))|youtu\\.be/)([a-zA-Z0-9_-]{6,11})"))

(defn select-youtube-links [tab]
  (let [url (:url tab)]
    (re-find youtube-regex url)))

(defn close-youtube-tabs [ids]
  (tabs/remove (to-array ids)))

(defn get-id [url]
  (let [[_, id] (re-find youtube-regex url)]
    id))

(defn prepare-playlist [urls]
  (let [ids (map get-id urls)
        playlist-url "http://www.youtube.com/watch_videos?video_ids="]
    (str playlist-url (string/join "," ids))))

(defn open-playlist [playlist-url]
  (tabs/create (clj->js {:url playlist-url})))

(defn build-youtube-playlist []
  (let [res (tabs/query (clj->js {:currentWindow true}))]
    (go
      (let [found-tabs (<! res)]
        (let [youtube-tabs (filter select-youtube-links (first (js->clj found-tabs :keywordize-keys true)))]
          (open-playlist (prepare-playlist (map :url youtube-tabs)))
          (close-youtube-tabs (map :id youtube-tabs)))))))

; -- main event loop --------------------------------------------------------------------------------------------------------

(defn process-chrome-event [event-num event]
  (log (gstring/format "BACKGROUND: got chrome event (%05d)" event-num) event)
  (let [[event-id event-args] event]
    (case event-id
      ::runtime/on-connect (apply handle-client-connection! event-args)
      ::tabs/on-created (tell-clients-about-new-tab!)
      ::browser-action/on-clicked (build-youtube-playlist)
      nil)))

(defn run-chrome-event-loop! [chrome-event-channel]
  (log "BACKGROUND: starting main event loop...")
  (go-loop [event-num 1]
           (when-some [event (<! chrome-event-channel)]
             (process-chrome-event event-num event)
             (recur (inc event-num)))
           (log "BACKGROUND: leaving main event loop")))

(defn boot-chrome-event-loop! []
  (let [chrome-event-channel (make-chrome-event-channel (chan))]
    (tabs/tap-all-events chrome-event-channel)
    (runtime/tap-all-events chrome-event-channel)
    (browser-action/tap-on-clicked-events chrome-event-channel)
    (run-chrome-event-loop! chrome-event-channel)))

; -- main entry point -------------------------------------------------------------------------------------------------------

(defn init! []
  (log "BACKGROUND: init")
  (test-storage!)
  (boot-chrome-event-loop!))
