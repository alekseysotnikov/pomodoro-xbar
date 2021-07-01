#!/usr/local/bin/bb
(defn notify! [notification sound]
  (shell/sh "osascript" "-e" (str "display notification \"" notification "\" with title \"🍅 Pomodoro\" sound name \"" sound "\"")))

(defn time-left [conf {started-millis :updated flow :flow}]
  (let [mode       (first flow)
        mode-max-seconds (* 60 (get-in conf [:modes mode :minutes] -1))
        left-seconds     (- mode-max-seconds (int (/ (- (System/currentTimeMillis) started-millis) 1000)))
        left-minutes     (+ 1 (int (/ left-seconds 61)))]
    (if (<= left-seconds 60)
      left-seconds
      left-minutes)))

(defn flow-duration [{modes :modes} flow]
  (reduce #(+ %1 (get-in modes [%2 :minutes] 0)) 0 flow))

(def conf {:state-path        (str (babashka.fs/temp-dir) "/clj-pomodoro-state")
           :default-state     {:flow    [:disabled]
                               :updated (System/currentTimeMillis)}
           :read-state        (fn [{:keys [state-path default-state]}]
                                (if (babashka.fs/exists? state-path)
                                  (-> state-path slurp read-string)
                                  default-state))
           :write-state       (fn [{state-path :state-path} old-state state]
                                (or (when-not (= old-state state)
                                      (let [state' (assoc state :updated (System/currentTimeMillis))]
                                        (spit state-path state')
                                        state'))
                                    state))
           :after-write-state (fn [conf {old-flow :flow} {flow :flow :as state}]
                                (when-not (= old-flow flow)
                                  (let [mode (first flow)
                                        {{{:keys [name sound icon]} mode} :modes} conf]
                                    (notify! (str icon " " name) sound)))
                                state)

           :state->next-state (fn [{modes :modes} {state-flow :flow :as state}]
                                (let [curent-mode (first state-flow)
                                      next-flow   (rest state-flow)
                                      state-flow' (if (empty? next-flow)
                                                    (get-in modes [curent-mode :flow] [:disabled])
                                                    next-flow)]
                                  (assoc state :flow state-flow')))

           :render            (fn [{:keys [modes state->next-state] :as conf}
                                   {:keys [flow] :as state}]
                                (let [mode       (first flow)
                                      next-mode (first (:flow (state->next-state conf state)))]
                                  (println (str (if (= :disabled mode)
                                                  "🍅"
                                                  (get-in modes [mode :icon]))
                                                (if (nat-int? (time-left conf state))
                                                  (format " %02d %s| color=%s"
                                                          (time-left conf state)
                                                          (apply str (map #(get-in modes [% :icon])
                                                                          [next-mode]))
                                                          (get-in modes [mode :color]))
                                                  "")
                                                "\n---\n"
                                                (->> (keys modes)
                                                     (map #(format "%s %s | bash=\"%s\" param1=\"%s\" terminal=false refresh=true\n"
                                                                   (get-in modes [%1 :icon])
                                                                   (get-in modes [%1 :name])
                                                                   ;(flow-duration conf (get-in modes [%1 :flow] []))
                                                                   *file*
                                                                   %1))
                                                     (apply str))
                                                "---\nFlow: "
                                                (apply str (map #(get-in modes [% :icon]) flow)))))
                                state)
           :modes             {:work       {:name    "Work"
                                            :flow    [:break]
                                            :sound   "Blow"
                                            :icon    "🍅"
                                            :color   "red"
                                            :minutes 25}
                               :break      {:sound   "Glass"
                                            :icon    "☕"
                                            :name    "Break"
                                            :color   "green"
                                            :flow    [:work]
                                            :minutes 5}
                               :long-break {:sound   "Glass"
                                            :icon    "🫖"
                                            :name    "Long Break"
                                            :color   "green"
                                            :flow    [:work]
                                            :minutes 20}
                               :work1      {:sound "Blow"
                                            :icon  "👔"
                                            :name  "Work 1h"
                                            :color "red"
                                            :flow  [:work :break :work :break :disabled]}
                               :work2      {:sound "Blow"
                                            :icon  "👔"
                                            :name  "Work 2h"
                                            :color "red"
                                            :flow  [:work :break :work :break :work :break :work :break :disabled]}
                               :work4      {:sound "Blow"
                                            :icon  "👔"
                                            :name  "Work 4h"
                                            :color "red"
                                            :flow  [:work :break :work :break :work :break :work :long-break
                                                    :work :break :work :break :work :break :work :disabled]}
                               :disabled   {:name  "Disable"
                                            :icon  "🔌"
                                            :sound "Disable"}}})

(let [{:keys [state->next-state
              render
              write-state
              after-write-state
              modes
              read-state]} conf
      {:keys [flow] :as state} (read-state conf)
      [mode-name & _] *command-line-args*
      state' (if mode-name
               (let [mode      (read-string mode-name)
                     mode-flow (cons mode (get-in modes [mode :flow] [:disabled]))]
                 (assoc state :flow mode-flow))
               (when (neg? (time-left conf state))
                 (state->next-state conf state)))]
  (->> (or state' state)
       (write-state conf state)
       (after-write-state conf state)
       (render conf))
  nil)



