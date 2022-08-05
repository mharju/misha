(ns core
  (:require [overtone.midi :as m])
  (:import [javax.swing JFrame JLabel]
           [java.awt Font GraphicsEnvironment]
           [java.awt.event KeyAdapter KeyEvent]
           [javax.sound.midi ShortMessage]))

(def ableton (m/midi-out))
(defn all-notes-off! []
  (m/midi-control ableton 0x7b 0))

(def actions
  {KeyEvent/VK_A {:offset -4} KeyEvent/VK_S {:offset -3} KeyEvent/VK_D {:offset -2}
   KeyEvent/VK_F {:offset -1} KeyEvent/VK_H {:offset 0} KeyEvent/VK_J {:offset 1}
   KeyEvent/VK_K {:offset 2} KeyEvent/VK_L {:offset 3} 16777462 {:offset 4}

   KeyEvent/VK_R :cycle-chord KeyEvent/VK_U :cycle-inversion KeyEvent/VK_E :cycle-scale
   KeyEvent/VK_W :notes-off})

(defn make-scale [notes]
  (let [notes (cycle notes)]
    (->> (iterate
           (fn [[x s]]
             [(+ x (first (take 1 (drop s notes)))) (inc s)]) [0 0])
         (take 100)
         (mapv first))))

(def scales
  ;; Cumulative
  {:ionian (make-scale [2 2 1 2 2 2 1])
   :pentatonic (make-scale [4 3 2 3])
   :whole (make-scale [2 2 2 2 2 2])
   :indian (make-scale [4 1 2 3 2])})

(def chord-modes
  {:triad (fn [index scale inversion]
            (->> scale
                 (drop index)
                 (take-nth 2)
                 (take 3)
                 (map-indexed
                  (fn [i n]
                    (if (>= (- 2 inversion) i)
                      (- n 12)
                      n)))))
   :quad (fn [index scale inversion]
           (->> scale
                (drop index)
                (take-nth 2)
                (take 4)
                (map-indexed
                  (fn [i n]
                    (if (>= (- 3 inversion) i)
                      (- n 12)
                      n)))))})

(def state (atom {:note 35 :previous-note 35 :scale :ionian :chord-mode :quad :inversion 1}))

(defn update! [action]
  (if (map? action)
    (when-let [offset (get action :offset)]
      (let [{:keys [note scale] :as prev-state} @state
            next-state (merge prev-state
                              {:note
                               (min (max (+ note offset) 0) (dec (count (get scales scale))))
                               :previous-note note})]
        (reset! state next-state)))
    (case action
      :cycle-inversion (swap! state update :inversion (fn [i]
                                                        (if (= :triad (:chord-mode @state))
                                                          (mod (inc i) 2)
                                                          (mod (inc i) 3))))
      :cycle-chord (swap! state update :chord-mode (fn [c]
                                                     (case c
                                                       :triad :quad
                                                       :quad nil
                                                       nil :triad)))
      :cycle-scale (reset! state
                           (-> @state
                               (update :scale (fn [s]
                                                (case s
                                                  :ionian :pentatonic
                                                  :pentatonic :whole
                                                  :whole :indian
                                                  :indian :ionian)))
                               (assoc :note 36)))
      :notes-off (all-notes-off!))))

(def keys-down (atom #{}))

(defn read-notes []
  (let [frame (JFrame. "Misha")
        font (Font. "DeftoneStylus-Regular" Font/PLAIN 42)
        label (JLabel. "Use the keyboard")
        listener (proxy [KeyAdapter] []
                   (keyPressed [event]
                     (let [code (.getKeyCode event)]
                       (when-not (@keys-down code)
                         (swap! keys-down conj code)
                         (when-let [action (get actions code)]
                           (update! action)
                           (when (map? action)
                             (let [{:keys [note scale chord-mode inversion]} @state
                                   current-scale (get scales scale)]
                               (if chord-mode
                                 (doseq [note ((get chord-modes chord-mode) note current-scale inversion)]
                                   (m/midi-note-on ableton note 100))
                                 (m/midi-note-on ableton (nth current-scale note) 100))))))))
                   (keyReleased [event]
                     (let [code (.getKeyCode event)]
                       (swap! keys-down disj code)
                       (when-let [action (get actions code)]
                         (when (map? action)
                           (let [{:keys [note scale chord-mode inversion]} @state
                                 current-scale (get scales scale)]
                             (if chord-mode
                               (doseq [note ((get chord-modes chord-mode) note current-scale inversion)]
                                 (m/midi-note-off ableton note))
                               (m/midi-note-off ableton (nth current-scale note)))))
                         ))))]
    (doto label
      (.setHorizontalAlignment JLabel/CENTER)
      (.setFont font))
    (doto frame
      (.add label)
      (.pack)
      (.setSize 400 200)
      (.setVisible true)
      (.addKeyListener listener))))

(defn -main [& _args]
  (read-notes))

(comment
  (read-notes))
