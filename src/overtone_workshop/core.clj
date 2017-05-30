(ns overtone-workshop.core
  (require [overtone.live :refer :all]
           [clojure.java.io :as io]
           [overtone.inst.drum :refer :all]))

;; utils
(defn resource [path]
  (.getPath (io/resource path)))

;; arrangement
(def bpm-120 (metronome 120))

;; melodies
(def rand-melody
  (mapv #(vector %1 %2)
        (shuffle (map find-note-name (scale :a2 :phrygian)))
        (cycle [1 0.5])))

(def upper-melody
  (mapv #(vector %1 %2)
        (shuffle (map find-note-name (scale :a4 :phrygian)))
        (cycle [2 3])))

(def pad-melody
  (mapv #(vector %1 %2)
        (shuffle (map find-note-name (scale :a4 :phrygian)))
        (cycle [5 10])))

(def subject [[:d4 2] [:a4 2] [:f4 2] [:d4 2] [:c#4 2] [:d4 1] [:e4 1] [:f4 2.5] [:g4 0.5] [:f4 0.5] [:e4 0.5] [:d4 1]])

(def subject-inf (lazy-seq (concat subject subject-inf)))
(def mixup (lazy-seq (concat (shuffle subject) mixup)))
(def rand-melody-inf (lazy-seq (concat (shuffle rand-melody) rand-melody-inf)))
(def upper-melody-inf (lazy-seq (concat (shuffle upper-melody) upper-melody-inf)))
(def pad-melody-inf (lazy-seq (concat (shuffle pad-melody) pad-melody-inf)))
(def kick-line
  (mapv #(vector %1 %2)
        [1 2 3 4]
        [1 2 3 4]))
(def kick-line (lazy-seq (concat [[1 1]] kick-line)))
(def hat-line (lazy-seq (concat [[1 1/4]] hat-line)))
;; sounds

(def ms-kick (load-sample (resource "casio-mt-100/00.wav")))
(def ms-hat (load-sample (resource "casio-mt-100/01.wav")))
(def ms-blip  (load-sample (resource "casio-mt-100/04.wav")))

(defsynth sampler [buf 0 rate 1]
  (let [snd (play-buf 1 buf)]
    (out 0 (pan2 (* snd 1)))))

;; begin

(def metro (metronome 140))
(def instruments {:kick  quick-kick
                  :snare noise-snare
                  :hit   soft-hat})

(def sequence {:kick  [1 0 0 1 1 0 0 0 1 0 0 0 0 0 0 0]
               :snare [0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0]
               :hit   [0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0]})

;; And now to some logic. play-sequence is a function with a beat count and a instrument
;; for parameters. The function will go through the next 4 beats and play the given
;; instrument at a future time.
(defn play-sequence [beat instr]
  (let [time (mod beat 4)
        seq (vec (take 4 (drop (* 4 time) (sequence instr))))]
    (if (= 1 (seq 0)) (at (metro (+ 0.00 beat)) ((instruments instr))))
    (if (= 1 (seq 1)) (at (metro (+ 0.25 beat)) ((instruments instr))))
    (if (= 1 (seq 2)) (at (metro (+ 0.50 beat)) ((instruments instr))))
    (if (= 1 (seq 3)) (at (metro (+ 0.75 beat)) ((instruments instr))))))

;; The player will iterate over the instruments and play the instruments sequence.
;; This function is recursive and will loop for ever.
(defn player [beat]
  (doseq [instr (keys instruments)] (play-sequence beat instr))
  (apply-at (metro (inc beat)) #'player (inc beat) []))


(demo 10 (buf-rd 1 ms-hat (+ (lf-tri 1.1) (*  (lin-lin (lf-tri 0.23) -1 1 0 1) (buf-frames ms-hat)))))
;; WOOWOWOWOW
(demo 10 (buf-rd 1 ms-hat (* (sin-osc 0.1) (buf-frames ms-hat))))

(looper bpm-120 (partial sampler kick) 1/4)


(play bpm-120 (partial sampler kick)  kick-line)

(stop)
(definst siny [freq 400 gate 0.2]
  (let [env (env-gen (perc) :action FREE)
        fenv (lin-exp (env-gen (perc 0 1) :action FREE) 0 1 200 5000)
        snd (mix (lf-tri (* freq [0.3 1 2])))]
    (distortion2 (rlpf (* (lag gate) env snd) fenv 0.3) 0.5)))
(siny)


(defn play-one
  [metronome beat instrument [pitch dur]]
  (let [end (+ beat dur)
        id (at (metronome beat)
               (if (keyword? pitch)
                 (instrument (midi->hz (note pitch)))
                 (instrument pitch)))]
    (at (metronome end) (ctl id :gate 0))
    end))

(defn play
  ([metronome inst score]
   (play metronome (metronome) inst score))
  ([metronome beat instrument score]
   (let [cur-note (first score)]
     (when cur-note
       (let [next-beat (play-one metronome beat instrument cur-note)]
         (apply-at (metronome next-beat) play metronome next-beat instrument
                   (next score) []))))))
(stop)
;; put it together
(play metro siny rand-melody-inf)
(play metro rich-sine upper-melody-inf)
(play metro pad pad-melody-inf)
(player (metro))
(stop)
(volume 3)
(definst rich-sine [freq 440 dur 1.0]
  (let [env (env-gen (perc 0.05 dur) :action FREE)
        env2 (lin-exp (env-gen (asr 0.05 0.2 dur) :action FREE) 0 1 500 10000)
        snd (mix (sin-osc (* freq [0.5 0.99 1.0 1.01 2/3])))]
    (rlpf (* snd env 0.8) env2 0.3)
    ))

(definst pad [freq 440 dur 1.0]
  (let [env (env-gen (perc 0.7 1.5) :action FREE)
        env2 (lin-exp (env-gen (asr 0.05 0.2 dur) :action FREE) 0 1 500 10000)
        snd (mix (sin-osc (* freq [0.5 1 2 3])))]
    (rlpf (* 0.5 snd env 0.8) env2 0.3)
    ))


(defsynth basic-channel [freq 220 duration 1]
  (let [filter-env (lin-exp (env-gen (perc 0.01 1 1 -9) :action FREE) 0 1 40 16000)
        mouse (lin-exp (mouse-y:kr 0 1) 0 1 40 16000)
        amp-env (env-gen (perc 0.2 duration 1 -9) :action FREE)
        ]
    (out 0 (pan2 (* amp-env (rlpf (+
                                   (square freq)
                                   (* 1/2 (square (* 1/2 freq)))) (lag filter-env (lin-rand)) 0.6)) (saw 1)))
    )
  )


;; fun with chords
(chord :c4 :minor)
(defn play-chord [chord instrument]
  (doseq [note chord] (instrument (midi->hz note)))
  )
(play-chord
 (chord :c4 :minor)
 siny)

(pad)
(stop)
