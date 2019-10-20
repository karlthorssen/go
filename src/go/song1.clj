(ns go.song3
  (:require [clojure.java.io :as io]
            [clojure.repl :refer [doc]]
            [go.boot :refer [connected]]
            [leipzig.melody :refer :all]
            [leipzig.live :as lz]
            [leipzig.scale :as scale]
            [leipzig.melody :as melody]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]
            [leipzig.scale :as scale]
            [overtone.inst.piano :as piano]
            [overtone.inst.synth :as synth]
            [overtone.inst.drum :as drum]
            [overtone.inst.sampled-piano :refer [sampled-piano]]
            [overtone.inst.sampled-flute :refer [sampled-non-vibrato-flute]]
            [overtone.inst.trumpet :refer [sampled-trumpet]]
            [go.inst :refer [organ bass sing wobble-organ supersaw my-piano
                             dub2 reese string bass2 organ2 plucky brass] :as
             my-inst]
            [overtone.core :refer :all :exclude [tap]]
            [go.play-inst :as player]))

(def semitone-ratio 1.059463)

(defmacro play-gated [duration & body]
  `(let [duration-ms# (* 1000 ~duration)
         inst-result# ~@body]
     (at (+ (now) duration-ms#)
         (ctl inst-result# :gate 0))))

(defmacro defperc [name path]
  `(let [buf# (sample ~path)]
     (definst ~name [~(symbol "amp") 1]
       (~'* ~(symbol "amp") (play-buf 2 buf# :action FREE)))))

(defn beats-to-time [n-beats bpm]
  (/ n-beats (/ bpm 60)))

(def vocal-buffer (sample "resources/ReverseVocal_C-D.wav"))
(definst vocal [ratio (Math/pow semitone-ratio 2)
                amp 1
                gate 1
                start-pos 0]
  (let [sample-rate (:rate vocal-buffer)]
    (* amp
       (env-gen:kr (adsr 0 0 1 0.1 1) gate :action FREE)
       (pitch-shift
        (scaled-play-buf 2 vocal-buffer :start-pos (* sample-rate start-pos) :action FREE)
        :pitch-ratio ratio
        :time-dispersion 0.01))))

(def sing-buffer (sample "resources/EndVocalLoop-16Bar_keyDMaj_120bpm.wav")) ;; definitely not D major
(definst sing-buffer-inst []
  (scaled-play-buf 2 sing-buffer :action FREE))
(definst sing-loop [ratio (/ 1 0.95) ;; cancel pitch shift from slowing down sample
                    amp 1
                    rate 0.95 ;; convert 120 bpm to 114
                    start-pos 0
                    attack 0.1
                    gate 1]
  (let [sample-rate (:rate sing-buffer)]
    (* amp
       (env-gen:kr (adsr attack 0 1 0.1 1) gate :action FREE)
       (pitch-shift
        (scaled-play-buf 2 sing-buffer :rate rate :start-pos (* rate sample-rate start-pos) :action FREE)
        :pitch-ratio ratio
        :time-dispersion 0.01))))

;; (play-gated (beats-to-time 8.0 BPM) (sing-loop :rate 0.85 :ratio (/ 1 0.85)))

(def chosen-scale (comp scale/C scale/major))
(def BPM 114)

(defn as-inst
  "applies the given instrument key to the notes and converts note
  indices to midi"
  [inst phrase]
  (->> phrase
       (wherever :pitch :pitch chosen-scale)
       (all :part inst)))

(defperc snare "resources/samples2/snare9.aif")
(defperc hat "resources/samples2/hatclosed3.aif")
(defperc hat2 "resources/samples2/hatclosed1.aif")
(defperc hat3 "resources/samples2/rimshot12.aif")
(defperc stick "resources/samples2/rimshot9.aif")
(defperc tambourine "resources/samples2/tambourine.aif")

#_(let [clav-sample (sample "resources/samples/PERCUSSION/Clav.wav")]
  (definst clav [amp 1]
    (* amp (pan2 :in (play-buf 1 clav-sample :action FREE) :pos 0))))

(def kit {:snare {:sound snare}
          :hat {:sound hat}
          :hat2 {:sound hat2}
          :hat3 {:sound hat3}
          :stick {:sound stick}
          :tambourine {:sound tambourine}
          :kick {:sound (fn [& {:keys [amp]}]
                          (my-inst/kick :amp amp :decay 0.3))
                 :amp 0.5}})

(defmethod lz/play-note :sing-loop [{:keys [start duration amp rate ratio]
                                     :or {amp 1 rate 0.95 ratio (/ 1 0.95)}}]
  (when start
    (let [sample-beat-length 64
          duration (or duration (- sample-beat-length start))]
      (play-gated duration
                  (sing-loop :start-pos (beats-to-time start BPM))))))

(defmethod lz/play-note :reverse-vocal [{:keys [start duration amp] :or {amp 1}}]
  (when start
    (let [sample-beat-length 8
          duration (or duration (- sample-beat-length start))]
      (play-gated duration
                  (vocal :start-pos (beats-to-time start BPM))))))

(defmethod lz/play-note :plucky [{:keys [pitch duration amp] :as note :or {amp 1}}]
  (when pitch
    (plucky :freq (temperament/equal pitch) :cutoff 900 :dur (* 0.8 duration) :amp amp)))

(defmethod lz/play-note :beat [{note-amp :amp drum :drum :or {note-amp 1}}]
  (let [{sound-fn :sound drum-amp :amp :or {drum-amp 0.04}} (get kit drum)]
    (when sound-fn
      (sound-fn :amp (* drum-amp note-amp)))))

(defmethod lz/play-note :bass [{:keys [pitch duration amp] :or {amp 1}}]
  (when pitch
   (play-gated duration
               (synth/vintage-bass :note pitch :velocity 30 :amp (* 0.35 amp)))))

(defmethod lz/play-note :organ [{:keys [pitch duration]}]
  (organ :freq (temperament/equal pitch) :dur duration))

(defmethod lz/play-note :organ2 [{:keys [pitch duration]}]
  (organ :freq (temperament/equal pitch) :dur duration))

(defmethod lz/play-note :sing [{:keys [pitch duration]}]
  (sing :freq (temperament/equal pitch) :dur duration))

(defmethod lz/play-note :brass [{:keys [pitch duration]}]
  (brass :note pitch :dur duration))

(defmethod lz/play-note :string [{:keys [pitch duration]}]
  (string :note pitch :dur duration))

(defmethod lz/play-note :piano [{:keys [pitch duration amp] :or {amp 1}}]
  (when pitch
    (play-gated (+ 0.2 duration)
     (sampled-piano pitch :level (* 0.12 amp) :release 0.3))))

(defmethod lz/play-note :piano2 [{:keys [pitch duration]}]
  (when pitch
    (play-gated duration
     (my-piano pitch :vel 60 :hard 0.2 :muffle 0.2 :velcurve 0.3))))

(defmethod lz/play-note :piano3 [{:keys [pitch duration]}]
  (when pitch
    (piano/piano pitch :vel 60 :velcurve 0.1 :decay 0.2)))

(defmethod lz/play-note :reese [{:keys [pitch duration]}]
  (let [release 0.02]
   (when pitch
     (reese :freq (temperament/equal pitch)
            :amp 0.08
            :dur (- duration release)))))

(defmethod lz/play-note :supersaw [{:keys [pitch duration amp] :or {amp 1} :as note}]
  (when pitch
    (supersaw (temperament/equal pitch) :amp (* amp 0.65) :dur (- duration 0.05) :release 0.1)))

(defmethod lz/play-note :trumpet [{:keys [pitch duration amp] :or {amp 1}}]
  (when pitch
   (play-gated duration
               #_(sampled-trumpet pitch :start-pos 0.25 :attack 0.01 :level (* amp 1.3))
               (sampled-trumpet pitch :attack 0.05 :level (* 0.3 amp) :start-pos 0.08))))

(defmethod lz/play-note :rest [_] nil)

(defmethod lz/play-note :dub-inst [{:keys [pitch]}]
  (dub2 :freq (temperament/equal  (- pitch 36))))

(defn tap [drum times length ]
  (map #(zipmap [:time :duration :drum]
                [%1 (- length %1) drum]) times))

(defn fade-out [notes]
  (let [a (apply min (map :time notes))
        b (apply max (map :time notes))
        fade (fn [n]
               (let [t (:time n)
                     x (/ (- t a)
                          (- b a))]
                 (update n :amp #(* (- 1 x) (or % 1)))))]
    (map fade notes)))

(defn fade-in [notes]
  (let [a (apply min (map :time notes))
        b (apply max (map :time notes))
        fade (fn [n]
               (let [t (:time n)
                     x (/ (- t a)
                          (- b a))]
                 (update n :amp #(* x (or % 1)))))]
    (map fade notes)))

(def chord-shift -7)
(def chords {:i (chord/root chord/triad (+ chord-shift 0))
             :i7 (chord/root chord/seventh (+ chord-shift 0))
             :ii (chord/root chord/triad (+ chord-shift 1))
             :iii (chord/root chord/triad (+ chord-shift 2))
             :iii7 (chord/root chord/seventh (+ chord-shift 2))
             :iii-inv (-> chord/triad (chord/root (+ chord-shift 2)) (chord/inversion 1))
             :v7 (-> chord/seventh (chord/root (+ chord-shift 4)))
             :v-maj7 (-> chord/seventh (chord/root (+ chord-shift 4))
                         (assoc :vii 3.5))
             :v7-inv (-> chord/seventh (chord/root 4) (chord/inversion 1))
             :v (-> chord/triad (chord/root (+ chord-shift 4)))
             :VM (-> chord/triad (chord/root (+ chord-shift 4))
                     (assoc :iii -0.5)) ;; Major chord
             :iv (chord/root chord/triad (+ chord-shift 3))
             :iv7 (chord/root chord/seventh (+ chord-shift 3))
             :vi (-> chord/triad (chord/root (+ chord-shift 5)))
             :vi7 (-> chord/seventh (chord/root (+ chord-shift 5)))})

(defn inst-phrase [inst times notes]
  (as-inst inst (phrase times notes)))

(def hf  2)
(def qtr 1)
(def eth 1/2)
(def sth 1/4)
(def swup (partial * 1.05))
(def swbk (partial * 0.95))

(def soft-drum (->> [(tap :hat (range 8) 8)
                     (tap :tambourine [1.75 2.5  3.5 5.75 6.5 7.5] 8)
                     (tap :hat2 [6.25 6.5 6.75] 8)
                     (tap :kick (range 0 8 2) 8)]
                    (reduce with)
                    (all :part :beat)))

(def steady-chords (inst-phrase :piano
                                [4 4 4 4]
                                (map chords [:i :i7 :vi7 :i7])))

(def steady-bass (inst-phrase :reese
                              [4
                               3 1
                               2  2
                               2 2]
                              [0
                               0 -1
                               -2 0
                               0 0]))

(def melody2 (inst-phrase :piano
                          [qtr qtr qtr qtr
                           qtr qtr qtr qtr
                           qtr qtr qtr qtr
                           qtr qtr eth eth eth eth]
                          [2 3 4 4
                           4 4 4 4
                           5 4 5 5
                           6 7 7 7 7 7]))

(def melody3 (inst-phrase :piano
                          (concat (repeat 8 qtr) [eth eth qtr qtr qtr] [qtr 3])
                          [1 2 2 2
                           1 2 2 2
                           1 1 1 1 1
                           0 -1]))

(def melody4 (inst-phrase :piano
                          [qtr qtr qtr qtr
                           qtr qtr qtr eth eth
                           qtr qtr qtr qtr
                           eth eth eth eth eth eth eth eth]
                          [2 3 4 4
                           4 4 6 4 4
                           5 4 5 5
                           6 7 6 7 7 7 7 7]))

(def chorus (with
                 (times 12 soft-drum)
                 (inst-phrase :bass
                              [4 2 1 1
                               4 4
                               16

                               32

                               4 4 4 4 4 4 4 4]
                              (map #(when % (+ -14 %))
                                   [nil 4 4 5
                                    6 nil
                                    nil

                                    nil

                                    0 2 3 3 0 -1 0 0]))
                 (inst-phrase :piano
                              [24
                               eth eth 5 qtr qtr

                               eth eth 3 hf qtr qtr
                               eth eth 5 qtr qtr
                               qtr 5 qtr qtr
                               qtr 5 qtr qtr

                               hf 4 qtr qtr
                               eth eth 5 qtr qtr
                               qtr 7
                               eth eth 5 eth eth qtr]
                              [nil
                               (chords :v-maj7) (chords :v-maj7) nil 6 4

                               (chords :iv) (chords :iv) nil 3 4 5
                               (chords :i)  (chords :i) nil 6 4
                               5 nil 5 4
                               3 nil 2 1

                               0 nil (chords :i7) (chords :i7)
                               (chords :i7) (chords :i7) nil -2 -1
                               (chords :i) nil
                               (chords :i) (chords :i) nil 0 1 0])
                 (inst-phrase :supersaw
                              [6 1 1
                               8
                               8
                               8

                               8 8 8 8

                               8 4 4 4 4 8]
                              (map chords [:v  :v7 :vi7
                                           :v7-inv
                                           :vi7
                                           :v-maj7

                                           :iv
                                           :i
                                           :vi
                                           :iv

                                           :i
                                           :i :iv
                                           :i :v
                                           :i]))))


(def min-drum (let [hat-pattern [0 1 1.75 2 2.25 2.5 2.75 3 3.5]]
                (->> [(tap :hat (range 8) 8)
                      (tap :tambourine (range 1 8 2) 8)
                      (tap :kick (range 0 8 2) 8)]
                     (reduce with)
                     (times 2)
                     (with (tap :hat [15.25 15.5 15.75] 8))
                     (all :part :beat))))

(def break-inst (->> min-drum
                      (with (inst-phrase :piano
                                         [4 4 4 4]
                                         (map chords [:ii :ii :vi :vi]))
                            (inst-phrase :reese
                                         [4 4 2 2 2 2]
                                         (map (partial + -14) [1 3 5 7 7 5])))
                      (times 2)))

(def sing-break (with (times 3 break-inst) [{:part :sing-loop :time 0 :start 0 :duration 64 :amp 1}
                                            {:part :sing-loop :time 64 :start 0 :duration 32 :amp 1}]))

(defn play! [notes]
  (->> notes (tempo (bpm BPM)) lz/play))

(def track (atom nil))
(do

  (reset! track
          (->>
           (with
            steady-chords
            (times 2 soft-drum)
            (inst-phrase :piano
                         (concat (repeat 8 qtr) [eth eth qtr qtr qtr] [qtr 3])
                         [1 2 2 2 1 2 2 2 1 1 1 1 1 0 -1]))
           (then
            (->> steady-chords
                 (with
                  (times 2 soft-drum)
                  [{:part :reverse-vocal :start 1 :duration 8 :time 4}]
                  melody3)))
           (then
            (with
             steady-chords
             steady-bass
             (times 2 soft-drum)
             melody2))
           (then
            (with
             steady-chords
             (times 2 soft-drum)
             melody4))
           (then chorus)

           (then
            (with
             steady-chords
             steady-bass
             (times 2 soft-drum)
             melody2))
           (then
            (->> steady-chords
                 (with
                  (times 2 soft-drum)
                  [{:part :reverse-vocal :start 1 :duration 8 :time 4}]
                  melody3)))
           (then
            (with
             steady-chords
             steady-bass
             (times 2 soft-drum)
             melody2))
           (then
            (with
             steady-chords
             (times 2 soft-drum)
             melody4))
           (then chorus)

           (then sing-break)
           (then chorus)

           (tempo (bpm BPM))))
  ;; (lz/play @track)
  ;; (time @(lz/play @track))
  nil
  )

(lz/stop)

(comment
  ;; i	ii	iii	iv	v	vi	vii
  ;; 0	1	2	3	4	5	6
  ;; -7	-6	5 	-4	-3	-2	-1
  (lz/jam track)
  (lz/stop)
  (time @(lz/play @track))
  (lz/stop)

  (player/play-reese chosen-scale)
  (player/play-piano2 chosen-scale)
  (player/play-trumpet chosen-scale)
  (player/play-supersaw chosen-scale)

  (play-gated 0.2 (synth/vintage-bass :note 33 :velocity 30))
  (supersaw :dur 4)


  (piano/piano 60)

  (do (recording-start "~/src/music/go/LifeAndBeyond.wav")
      @(lz/play @track)
      (recording-stop))

  (let [b (piano/piano 60 :dur 10)]
    (at (+ (now) 200)
        (kill b))
    nil)

  (synth/ks1 :coef 0.8 :decay 20)
  (synth/bubbles)
  (kill synth/bubbles)


  (play-gated 0.2 (sampled-trumpet 60 :attack 0.01 :decay 0.29  :level 10 :sustain 0.1 :start-pos 500))
  (play-gated 1 (sampled-piano 60))
  (play-gated 1 (sampled-trumpet 60 :start-pos 0.25 :attack 0.01 :level 1.3))
)
(println ":::::::::::::::::::::::::::::: loaded :::::::::::::::::::::::::::::")
