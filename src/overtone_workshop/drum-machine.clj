(ns overtone-workshop.core
  (require [overtone.live :refer :all]
           [clojure.java.io :as io]
           [overtone.inst.drum :refer :all]))

(def ms-kick (load-sample (resource "casio-mt-100/00.wav")))
(def hat (load-sample (resource "casio-mt-100/01.wav")))
(def ms-blip  (load-sample (resource "casio-mt-100/04.wav")))

(defsynth sampler [buf 0 rate 1]
  (let [snd (play-buf 1 buf)]
    (out 0 (pan2 (* snd 1)))))

;; begin

(def metro (metronome 140))

(def instruments {:kick  quick-kick
                  :snare noise-snare
                  :hit   soft-hat})

(def sequence {:kick  [1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
               :snare [0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0]
               :hit   [1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0]})

;; And now to some logic. play-sequence is a function with a beat count and a instrument
;; for parameters. The function will go through the next 4 beats and play the given
;; instrument at a future time.
(defn play-sequence [beat instr]
  (let [time (mod beat 4)
        seq (vec (take 4 (drop (* 4 time) (sequence instr))))]
    (if (= 1 (seq 0)) (at (metro (+ 0 beat)) ((instruments instr))))
    (if (= 1 (seq 1)) (at (metro (+ 1/4 beat)) ((instruments instr))))
    (if (= 1 (seq 2)) (at (metro (+ 2/4 beat)) ((instruments instr))))
    (if (= 1 (seq 3)) (at (metro (+ 3/4 beat)) ((instruments instr))))))

(vec (take 4 (drop (* 4 0) (sequence :hit))))
(at (metro (+ 0.50 0)) ((instruments :kick)))

;; The player will iterate over the instruments and play the instruments sequence.
;; This function is recursive and will loop for ever.
(defn player [beat]
  (doseq [instr (keys instruments)] (play-sequence beat instr))
  (apply-at (metro (inc beat)) #'player (inc beat) []))

(player (metro))
(stop)
