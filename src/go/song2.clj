(ns go.song2
  (:require [clojure.java.io :as io]
            [clojure.repl :refer [doc]]
            [clojure.string :as str]
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
            [overtone.inst.sampled-piano :refer [sampled-piano]]
            [overtone.core :refer :all :exclude [tap]]
            [go.play-inst :as player]))


(def samples (into {}
                   (for [fp
                         ["resources/2019-10-samples/Melodic/LowString_C.wav"
                          "resources/2019-10-samples/Melodic/MelodicLoop 02.wav"
                          "resources/2019-10-samples/Melodic/MelodicLoop-02-pitch-shift.wav"
                          "resources/2019-10-samples/Melodic/MelodicLoop 01.wav"
                          "resources/2019-10-samples/Melodic/MoogSaw.wav"
                          "resources/2019-10-samples/Melodic/Harp_C.wav"
                          "resources/2019-10-samples/Melodic/Vocal_C.wav"
                          "resources/2019-10-samples/Melodic/Stab_D.wav"
                          "resources/2019-10-samples/Melodic/Stab_G.wav"
                          "resources/2019-10-samples/FX/Gasp.wav"
                          "resources/2019-10-samples/FX/FX 01.wav"
                          "resources/2019-10-samples/FX/FX 02.wav"
                          "resources/2019-10-samples/Drums/Snare 02.wav"
                          "resources/2019-10-samples/Drums/Snare 03.wav"
                          "resources/2019-10-samples/Drums/Snare 01.wav"
                          "resources/2019-10-samples/Drums/AmenBreak.wav"
                          "resources/2019-10-samples/Drums/Loop 03.wav"
                          "resources/2019-10-samples/Drums/Loop 02.wav"
                          "resources/2019-10-samples/Drums/Loop 01.wav"
                          "resources/2019-10-samples/Drums/Hat 02.wav"
                          "resources/2019-10-samples/Drums/Hat 03.wav"
                          "resources/2019-10-samples/Drums/Hat 01.wav"
                          "resources/2019-10-samples/Drums/Perc 03.wav"
                          "resources/2019-10-samples/Drums/Hat 04.wav"
                          "resources/2019-10-samples/Drums/Kick 02.wav"
                          "resources/2019-10-samples/Drums/Perc 02.wav"
                          "resources/2019-10-samples/Drums/Kick 01.wav"
                          "resources/2019-10-samples/Drums/Perc 01.wav"
                          "resources/2019-10-samples/Drums/Break 02.wav"
                          "resources/2019-10-samples/Drums/Break 01.wav"
                          "resources/2019-10-samples/Bass/Bass_C#.wav"
                          "resources/2019-10-samples/Bass/Bass_D 02.wav"
                          "resources/2019-10-samples/Bass/Bass_D 01.wav"
                          "resources/2019-10-samples/Vocal/Breath 01.wav"
                          "resources/2019-10-samples/Vocal/Vocal 01.wav"
                          "resources/2019-10-samples/Vocal/Vocal 03.wav"
                          "resources/2019-10-samples/Vocal/Vocal 02.wav"
                          "resources/2019-10-samples/Vocal/Chewie.wav"
                          "resources/2019-10-samples/Vocal/Vocal 04.wav"
                          "resources/2019-10-samples/Foley/Coins 01.wav"
                          "resources/2019-10-samples/Foley/Vinyl.wav"
                          "resources/2019-10-samples/Foley/FaceLift.wav"
                          "resources/2019-10-samples/Foley/FoleyPerc 01.wav"]]
                     [(last (str/split fp #"/")) (sample fp)])))

(comment
  ((samples "Hat 04.wav"))

  (do
    ((samples "Vocal_C.wav") :rate 1)
    ((samples "Vocal_C.wav") :rate (Math/pow semitone-ratio 4))
    ((samples "Vocal_C.wav") :rate (Math/pow semitone-ratio -3)))

  (do ((samples "Bass_D 02.wav") :rate 1 :amp 0.3 :start-pos 0)
      ((samples "Bass_D 01.wav") :rate 1)
      )

  (do
    ((samples "Bass_D 01.wav") :rate 1)
    ((samples "MelodicLoop 02.wav") :rate 1))

  (do
    ((samples "Bass_D 01.wav") :rate 1)
    ((samples "MelodicLoop-02-pitch-shift.wav") :rate (/ 150 171.0)))

  ((samples "Stab_D.wav") :rate 1)
  ((samples "Stab_G.wav") :rate 1)
  ((samples "Vocal_C.wav") :rate 1)
  ((samples "Harp_C.wav") :rate 1)
)

(def chosen-scale (comp scale/F scale/major))
(def BPM 150)

(defmacro def-sample-inst
  ([inst-name sample-name] `(def-sample-inst ~inst-name ~sample-name true))
  ([inst-name sample-name shifted?]
   `(let [buf# (samples ~sample-name)]
      (definst ~inst-name [~'amp 1
                           ~'rate 1
                           ~'pitch-ratio 1
                           ~'start-pos 0
                           ~'attack 0
                           ~'decay 0
                           ~'sustain 1
                           ~'release 0.1
                           ~'curve ~LINEAR
                           ~'dur 2
                           ~'gate 1]
        (let [sample-rate# (:rate buf#)
              env-gate# (~'* ~'gate (line:kr 1 0 ~'dur))]
          (~'* ~'amp
           (env-gen:kr (adsr ~'attack ~'decay ~'sustain ~'release 1 ~'curve) env-gate# :action FREE)
           (if ~shifted?
             (pitch-shift
              (scaled-play-buf 2 buf# :start-pos (~'* sample-rate# ~'start-pos) :rate ~'rate :action FREE)
              :pitch-ratio ~'pitch-ratio
              :time-dispersion 0.1)
             (scaled-play-buf 2 buf# :start-pos (~'* sample-rate# ~'start-pos) :rate ~'rate :action FREE))))))))

(def-sample-inst moog-saw "MoogSaw.wav")
(def-sample-inst low-string "LowString_C.wav")

(def-sample-inst stab-g "Stab_G.wav")
(def-sample-inst vocal "Vocal_C.wav")
(def-sample-inst harp "Harp_C.wav")
(def-sample-inst gasp "Gasp.wav")
(def-sample-inst rise "FX 01.wav")


(let [buf (samples "Stab_D.wav")
      sample-rate (:rate buf)]
  (definst stab-d [amp 1 pitch-ratio 1 rate 1 dur 1]
    (let [env-gate (line:kr 1 0 dur)]
      (* amp
         (env-gen:kr (adsr 0 0 1 0 1 LINEAR) env-gate :action FREE)
         (pitch-shift
          (scaled-play-buf 2 buf :rate rate :action FREE :start-pos (* 0.1 sample-rate))
          :pitch-ratio pitch-ratio
          :time-dispersion 0.01)))))

(let [buf (samples "MelodicLoop 02.wav")
      sample-rate (:rate buf)]
  (definst melodic2 [amp 1 start-pos 0 dur 6 lpf-start 0.5 lpf-end 1.0]
    (let [bpm-ratio (/ BPM 128)]
      (->
       (* amp
          (env-gen:kr (adsr 0 0.25 (/ 1 8) 0 8 LIN) (line:kr 1 0 dur) :action FREE)
          (pitch-shift
           (scaled-play-buf 2 buf :start-pos (* sample-rate start-pos) :rate bpm-ratio :action FREE)
           :pitch-ratio (/ 1 bpm-ratio) :time-dispersion 0.01))

       (rlpf (env-gen (envelope [lpf-start lpf-end] [16] :sqr) :level-scale 1500) 0.2)
       (free-verb :mix 0.3)))))



(let [buf1 (samples "Bass_D 01.wav")
      buf2 (samples "Bass_D 02.wav")
      d1-freq 36.71]
  (definst bass [amp 1 freq 55 dur 2]
    (let [snd1 (* 1.3 (scaled-play-buf 2 buf1 :action FREE))
          pitch-ratio (/ freq d1-freq)
          snd2 (-> (scaled-play-buf 2 buf2 :rate 1/2 :action FREE)
                   (lpf 52)
                   (* (env-gen (perc 0.01 0.2 (/ 1.5 (squared pitch-ratio)) LIN))))
          snd-all-dirty (pitch-shift (+ snd1 snd2) :pitch-ratio pitch-ratio
                                     :pitch-dispersion 0
                                     :window-size (/ 0.1 (+ 1 (* 2 (log pitch-ratio))))
                                     :time-dispersion (/ 0.1 (+ 1 (* 2 (log pitch-ratio)))))
          snd-all (-> (+ (bpf snd-all-dirty freq 0.2) (bpf snd-all-dirty (* 2 freq) 0.2))
                      (lpf (* 5 freq))
                      (* (env-gen:kr (adsr 0 0.25 (/ 1 4) 0 4 LIN) (line:kr 1 0 dur) :action FREE)
                         amp))]
      (compander :in snd-all :control snd-all :thresh 0.4 :slope-below 0.7 :slope-above 0.5 :clamp-time 0.001)))
  #_(play! (with (all :amp 0.5 (times 4 boom-bap))
               bass1)))

(let [buf (samples "MoogSaw.wav")
      c3-freq 130.8]
  (definst moog-supersaw [freq 440 dur 0.2 release 0.5 amp 1.0 cutoff 3500 gate 1]
    (let [snd-fn (fn [freq]
                   (let [tune (ranged-rand 0.99 1.01)]

                     (pitch-shift
                      (scaled-play-buf 2 buf
                                       :loop true
                                       :start-pos 0
                                       :rate 4
                                       :action FREE)
                      :pitch-ratio (/ freq c3-freq)
                      :time-dispersion 0.1)


                     (-> (lf-saw (* freq tune))
                         (delay-c 0.005 (ranged-rand 0.0001 0.01)))))
          hi-saws (splay (repeatedly 5 #(snd-fn freq)))
          lo-saws (splay (repeatedly 5 #(snd-fn (/ freq 2))))
          snd (+ (* 0.65 hi-saws) (* 0.85 lo-saws))
          env (env-gen (adsr 0.001 0.7 0.2 0.1)
                       (* (env-gen (env-asr 0 1 release) gate) (line:kr 1 0 (+ dur release)))
                       :action FREE)]
      (-> snd
          (clip2 0.45)
          (rlpf (+ freq (env-gen (adsr 0.001) (* gate (line:kr 1 0 dur)) :level-scale cutoff)) 0.75)
          (free-verb :room 1.0 :mix 0.45)
          (* env amp)
          pan2))))

(def semitone-ratio 1.059463)

(defmacro play-gated [duration & body]
  `(let [duration-ms# (* 1000 ~duration)
         now# (now)
         inst-result# ~@body]
     (at (+ now# duration-ms#)
         (ctl inst-result# :gate 0))))

(defmacro defperc [name path]
  `(let [buf# (sample ~path)]
     (definst ~name [~(symbol "amp") 1]
       (~'* ~(symbol "amp") (play-buf 2 buf# :action FREE)))))

(defn beats-to-time [n-beats bpm]
  (/ n-beats (/ bpm 60)))

(defn as-inst
  "applies the given instrument key to the notes and converts note
  indices to midi"
  [inst phrase]
  (->> phrase
       (wherever :pitch :pitch chosen-scale)
       (all :part inst)))

(defperc snare "resources/2019-10-samples/Drums/Snare 01.wav")
(defperc kick "resources/2019-10-samples/Drums/Kick 01.wav")
(defperc hat "resources/2019-10-samples/Drums/Hat 01.wav")
(defperc ha2 "resources/2019-10-samples/Drums/Hat 02.wav")
(defperc hat3 "resources/2019-10-samples/Drums/Hat 03.wav")
(defperc hat4 "resources/2019-10-samples/Drums/Hat 04.wav")

(def kit {:kick {:sound kick}
          :snare {:sound snare}
          :hat {:sound hat}
          :hat4 {:sound hat4}
          :gasp {:sound (fn [& we] (gasp :rate 2 :pitch-ratio 0.5))}})


(defmethod lz/play-note :beat [{note-amp :amp drum :drum :or {note-amp 1}}]
  (let [{sound-fn :sound drum-amp :amp :or {drum-amp 0.5}} (get kit drum)]
    (when sound-fn
      (sound-fn :amp (* drum-amp note-amp)))))

(defmethod lz/play-note :low-string [{:keys [pitch duration amp] :or {amp 1} :as note}]
  (when pitch
   (let [pitch-ratio (Math/pow semitone-ratio (- pitch 60))]
     (low-string :start-pos 0.9 :attack 0.1 :release 0.3
                 :amp amp
                 :dur duration
                 :pitch-ratio pitch-ratio))))

(defmethod lz/play-note :vocal [{:keys [pitch duration amp] :or {amp 1} :as note}]
  (when pitch
   (let [pitch-ratio (Math/pow semitone-ratio (- pitch 60))]
     (vocal :amp (* 1.3 amp)
            :dur duration
            :pitch-ratio pitch-ratio))))

(defmethod lz/play-note :harp [{:keys [pitch duration amp] :or {amp 1} :as note}]
  (when pitch
   (let [pitch-ratio (Math/pow semitone-ratio (- pitch 60))]
     (harp
      :dur duration
      :rate 2
      :attack 0.01
      :decay duration
      :curve EXP
      :pitch-ratio (/ pitch-ratio 2)))))

(defmethod lz/play-note :rise [{:keys [duration amp] :or {amp 1} :as note}]
  (rise :rate -0.7 :start-pos 7.49 :dur duration :pitch-shift (/ 1 0.7)))

(do
  (defmethod lz/play-note :stab-d [{:keys [pitch duration amp] :or {amp 1} :as note}]
    (when pitch
      (let [pitch-ratio (Math/pow semitone-ratio (- pitch 62))]
        (doseq [x [0.998 0.999 0.9995 1 1.0005 1.001 1.002]]
          (stab-d
           :pitch-ratio (* 1/3 pitch-ratio x)
           :rate 3
           :amp 0.5
           :release duration
           :dur (/ duration 2))))))
  (play! (with (all :part :stab-d melody2) (times 2 boom-bap))))

(defmethod lz/play-note :stab-g [{:keys [pitch duration amp] :or {amp 1} :as note}]
  (when pitch
    (let [pitch-ratio (Math/pow semitone-ratio (- pitch 67))]
      (doseq [x [0.998 0.999 0.9995 1 1.0005 1.001 1.002]]
        (stab-g
         :pitch-ratio (* 2 pitch-ratio x)
         :rate 1/2
         :release 0.2
         :amp 0.5
         :dur (/ duration 2))))))

(defmethod lz/play-note :bass [{:keys [pitch duration amp] :or {amp 1} :as note}]
  (when pitch
   (let [freq (temperament/equal (- pitch 36))]
     (bass
      :amp amp
      :freq freq
      :dur duration))))

(defmethod lz/play-note :supersaw [{:keys [pitch duration amp] :or {amp 1} :as note}]
  (when pitch
    (moog-supersaw (temperament/equal pitch) :amp (* amp 1.2) :dur (- duration 0.05) :release 0.1)))

(defmethod lz/play-note :melodic [{:keys [duration amp lpf-start lpf-end] :or {amp 1 lpf-start 0.5 lpf-end 1.0} :as note}]
  (melodic2 :amp amp :dur duration :lpf-start lpf-start :lpf-end lpf-end))

(defmethod lz/play-note :rest [_] nil)

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
             :ii7 (chord/root chord/seventh (+ chord-shift 1))
             :iii (chord/root chord/triad (+ chord-shift 2))
             :iii7 (chord/root chord/seventh (+ chord-shift 2))
             :iii-inv (-> chord/triad (chord/root (+ chord-shift 2)) (chord/inversion 1))
             :v7 (-> chord/seventh (chord/root (+ chord-shift 4)))
             :v-maj7 (-> chord/seventh (chord/root (+ chord-shift 4))
                         (assoc :vii 3.5))
             :v7-inv (-> chord/seventh (chord/root 4) (chord/inversion 1))
             :v (-> chord/triad (chord/root (+ chord-shift 4)))
             :VM (-> chord/triad (chord/root (+ chord-shift 4))
                     (assoc :iii -0.5)) ;; Major dominant chord in minor key
             :iv (chord/root chord/triad (+ chord-shift 3))
             :iv7 (chord/root chord/seventh (+ chord-shift 3))
             :vi (-> chord/triad (chord/root (+ chord-shift 5)))
             :vi7 (-> chord/seventh (chord/root (+ chord-shift 5)))})

(defn inst-phrase [inst times notes]
  (as-inst inst (phrase times notes)))

(defn play! [notes]
  (->> notes (tempo (bpm BPM)) lz/play))

(def hf  2)
(def qtr 1)
(def eth 1/2)
(def sth 1/4)
(def swup (partial * 1.05))
(def swbk (partial * 0.95))

(def boom-bap (->> [(tap :kick [0 4] 8)
                    (tap :snare [2 6] 8)
                    (all :amp 0.25 (tap :hat4 (sort (range 1 8 2)) 8))]
                   (reduce with)
                   (all :part :beat)))

(def ticks (->> [(tap :kick [0 4] 8)
                 (all :amp 0.25 (tap :hat4  (range 0 8 eth) 8))
                 (all :amp 0.25 (tap :hat [0 1 1.5 2 3 4 4.5 5 5.5 6 7 7.5 ] 8))
                 (tap :snare [2 6] 8)]
                   (reduce with)
                   (all :part :beat)
                   (where :amp #(* (or % 1) 0.5))))

(def phrase1 (inst-phrase :supersaw
                          [[qtr qtr qtr] qtr [qtr qtr qtr] qtr [qtr qtr] [hf qtr qtr] hf]
                          (map chords [:vi nil :iii7 nil :ii7 :v7 nil])))


(def melody1-fadein [{:part :melodic :time 0 :duration 32}])
(def melody1 [{:part :melodic :time 0 :duration 32 :lpf-end 1.0 :lpf-start 1.0}])

(def melody2 (inst-phrase :stab-d (repeat 32 eth) (concat (repeat 8 5)
                                                          (repeat 8 7)
                                                          (repeat 8 8)
                                                          [6 7 8 10 6 6 6 6])))

(def bass1
  (inst-phrase :bass
               [4 2 2
                4 1 2 1
                4  2 2
                4  2 2 ]
               [-2 nil 2
                nil nil -1 -3
                nil -1 nil
                0 -2 0]))

(def bass2
  (inst-phrase :bass
               [4 4
                4 2 2
                4 1 2 1
                4 2 2]
               [-2 nil
                nil nil -3
                nil nil 1 2
                nil -3 -1]))

(def track (atom nil))
(do
  (reset! track
          (->> melody1-fadein
               (then (with (times 4 boom-bap) melody1))
               (then (with (times 4 boom-bap) melody1))
               (then (with (times 4 boom-bap) melody1 bass1
                           [{:part :rest :time 0 :duration 16}
                            {:part :rise :time 16 :duration 16}]))

               (then (with (times 4 boom-bap) melody1 bass1
                           (inst-phrase :vocal [4 4] [-2 -1])))
               (then (with (times 4 boom-bap) melody1 bass1))
               (then (with (times 4 boom-bap) melody1 bass1
                           [{:part :vocal :time 30 :duration 2 :pitch 65}]))
               (then
                (with (times 4 boom-bap)
                      [{:part :vocal :time 16 :duration 4 :pitch 67}]
                      [{:part :melodic :time 0 :duration 24 :lpf-end 1.0 :lpf-start 1.0}]
                      (filter #(< (:time %) 24) bass1)
                      [{:part :rise :time 0 :duration 32}]))

               (then (with (then (times 2 ticks) (times 2 boom-bap))
                           (inst-phrase :vocal [2 2] [[7 9] [7 5]])
                           (times 2 phrase1)
                           bass2))
               (then
                (with (times 4 ticks) bass2
                      (inst-phrase :vocal [4 24 4] [[9 7] nil [3 4]])
                      (->> melody2
                           (then
                            (but 12 16 (inst-phrase :stab-d (repeat 8 eth) [4 4 4 5 6 6 6 6]) melody2)))))
               (then (with (inst-phrase :stab-d (repeat 4 eth) (repeat 4 7))
                           (times 2 ticks)
                           (inst-phrase :bass [4 4 1 2 1 2 2] [0 0 nil -2 -3 nil -2])
                           (inst-phrase :vocal [8 8] [[0 2] [-2 0]])))
               (then (with (times 2 ticks)
                           (inst-phrase :bass [4 4 1 2 1 2 2] [0 0 nil -2 -3 nil -2])
                           (inst-phrase :vocal [8 8] [[0 2] [-2 0]])))
               (then (with (->> ticks (then (times 7 boom-bap)))
                           [{:part :melodic :time 0 :duration 32 :lpf-start 1.0 :lpf-end 0.5}
                            {:part :melodic :time 32 :duration 32 :lpf-start 0.5 :lpf-end 0.2}]
                           (->> bass1 (then (fade-out bass1)))))
               (tempo (bpm BPM))))
  (lz/play @track)
  ;; (time @(lz/play @track))
  nil
  )

(lz/stop)

(play!
 (->> (with (times 2 boom-bap)
            phrase1)
      (then
       (with (times 4 ticks) bass2
             (inst-phrase :vocal [4 24 4] [[9 7] nil [3 4]])
             (->> melody2
                  (then
                   (but 12 16 (inst-phrase :stab-d (repeat 8 eth) [4 4 4 5 6 6 6 6]) melody2)))))))



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


  (do (recording-start "~/src/music/go/Lost Arcadia.wav")
      @(lz/play @track)
      (Thread/sleep 2000)
      (recording-stop))

  (do (recording-start "~/src/music/go/stab-test.wav")
      @(play! (take 2 (with (all :part :stab-d melody2) (times 2 boom-bap))))
      (Thread/sleep 2000)
      (recording-stop))
)
(println ":::::::::::::::::::::::::::::: loaded :::::::::::::::::::::::::::::")
