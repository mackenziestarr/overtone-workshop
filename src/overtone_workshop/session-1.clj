(ns overtone-workshop.core
  (require [overtone.live :refer :all]))

;; ex. 1 -- COMMON OSCILLATORS --
(demo (sin-osc))
(demo (lf-saw))
(demo (square))
(demo (lf-tri))

;; second argument is frequency
(demo (lf-tri 220))

;; ex. 2 -- THE IMPORTANCE OF SINE --
;; make a sawtooth wave out of 100 sine waves via list comprehension,
;; like your precious python 🐍
(demo
 (let [* overtone.sc.ugen.collide/*]
   (sum (for [i (range 1 100)]
          (* (/ 1 i) (sin-osc (* i 220)))))))

;; additive synthesis
;; http://quod.lib.umich.edu/cgi/p/pod/dod-idx/synthesizing-a-javanese-gong-ageng.pdf
(definst bell [frequency 440 duration 1.0 volume 1.0 position 0 wet 0.5 room 0.5
               h0 1 h1 0.6 h2 0.4 h3 0.25 h4 0.2]
  (let [harmonics   [ 1  2  3  4.2  5.4]
        proportions [h0 h1 h2   h3   h4]
        proportional-partial
        (fn [harmonic proportion]
          (let [envelope (* 1/5 (env-gen (perc 0.01 (* proportion duration))))
                overtone (* harmonic frequency)]
            (* 1/2 proportion envelope (sin-osc overtone))))
        partials
        (map proportional-partial harmonics proportions)
        whole (* 10 (mix partials))]
    whole))


;; ex. 3 -- SHOWING THE FREQUENCY - PITCH CORRELATION
;; good chance to show off doseq. functions that start with 'do' in
;; clojure are often used cordone off the outside world (I/O thingers)
;; from the rest of your application.
(doseq [octave [200 400 800 1600]]
  (demo 0.2 (sin-osc octave))
  (Thread/sleep 200))

;; ex. 4 -- FILTERS --

;; lowpass
(do
  (demo 1 (lf-saw))
  (Thread/sleep 1000)
  (demo 1 (lpf (lf-saw) 500)))

;; hipass
(do
  (demo 1 (lf-saw))
  (Thread/sleep 1000)
  (demo 1 (hpf (lf-saw) 2000)))

;; bandpass
(do
  (demo 1 (lf-saw))
  (Thread/sleep 1000)
  (demo 1 (bpf (lf-saw) 1000 0.1)))

;; way more fun when you move the cutoff frequency around
(demo 5 (lpf (lf-saw) (mouse-x 20 6000 EXP)))

;; ex. 5 -- ENVELOPES --

;; perc
(demo 2 (*
         (env-gen (perc) :action FREE)
         (sin-osc)))

;; triangle
(demo 2 (*
         (env-gen (triangle) :action FREE)
         (sin-osc)))

;; adsr
(demo 5 (*
         (env-gen (adsr 0.01 1 0.8 2) (trig 1) :action FREE)
         (sin-osc)))

;; amplitude + pitch
(demo 5 (let [env (env-gen (adsr 0.01 1 0.8 2) (trig 1) :action FREE)]
          (* env (sin-osc (+ 400 (* env 2000))))))


;; ex. 6 -- LFOS --
(comment
  (mul-add in 20 100)
  "is equivalent to"
  (+ 100 (* in 20)))

;; classic lfo business
;; frequency
(demo 5 (sin-osc (mul-add (sin-osc 0.8) 20 200)))
;; amplitude
(demo 5 (* (+ 1 (sin-osc 0.5)) (sin-osc)))
;; all together now
(demo 5 (let [lfo-amp (+ 1 (sin-osc 1))
              lfo-freq (mul-add (sin-osc 2) 20 200)]
          (* lfo-amp (sin-osc lfo-freq))))

;; audio rate modulation (FM synthesis)
(demo 15 (sin-osc (mul-add (sin-osc (mouse-x 0.2 2000 EXP)) 100 400)))
(demo 15 (sin-osc (mul-add (square (mouse-x 0.2 2000 EXP)) 100 400)))


;; ex. 7 -- definst --


;; definst hello world
(definst my-sine [freq 440]
  (sin-osc freq))

(my-sine)
(stop)

;; lets try that again
(def my-sine-instance (my-sine))

my-sine-instance
(stop)

;; have some control
(def my-sine-instance (my-sine))
(ctl my-sine-instance :freq 800)
(stop)

;; more complicated definst
(definst my-filtered-saw [freq 440]
  (lpf (lf-saw) (mouse-x 40 2000 EXP)))
(my-filtered-saw)
(stop)

;; thats definst!
(definst random-pitch-sine [foobar 4]
  (let [freq (lin-exp (lf-noise0 foobar) -1 1 200 1000)
        amp  (lin-exp (lf-noise1 12) -1 1 0.02 1)]
    (* amp (sin-osc freq))))
(random-pitch-sine)
(stop)

;; -ex. 8 --STEREO & MULTICHANNEL --
;; defsynth is like definst with some more complicated shit going on
(defsynth pulse-test [ampHz 2 fund 400 maxPartial 8 width 0.5]
  (let [amp1 (* 0.75 (lf-pulse:kr ampHz 0 0.12))
        amp2 (* 0.75 (lf-pulse:kr ampHz 0.5 0.12))
        freq1 (round (lin-exp (lf-noise0:kr 4) -1 1 fund (* maxPartial fund)) 1)
        freq2 (round (lin-exp (lf-noise0:kr 4) -1 1 fund (* maxPartial fund)) 1)
        sig1 (free-verb (* amp1 (pulse freq1 width)) 0.7 0.8 0.25)
        sig2 (free-verb (* amp2 (pulse freq2 width)) 0.7 0.8 0.25)
        ]
    (out 0 sig1)
    (out 1 sig2)))
(pulse-test)

;; ✧・ﾟ: *✧・ﾟ:* *:・ﾟ✧*:・ﾟ✧ .・゜゜・ ・゜゜・． ｡・ﾟﾟ・
;; ✧・ﾟ: *:・JUMP ON IT, LETS DO IT・ ・゜゜・． ｡・ﾟﾟ・ ・ﾟﾟ・｡
;; ✧・ﾟ: *✧・ﾟ:* *:・ﾟ✧*:・ﾟ✧ .・゜゜・ ・゜゜・． ｡・ﾟﾟ・ ・ﾟﾟ

(definst pony [note 36 gate 1]
  (let [amp (env-gen (asr 0.01 1 0.5) gate :action FREE)
        snd (var-saw (midicps note) 0 0.95)
        filter-frequency (lin-exp (env-gen (sine 0.8)) 0 1 200 800)
        filter (rlpf snd filter-frequency 0.2)
        sample-and-hold (latch:ar filter (sin-osc 2000))]
    (* amp sample-and-hold)
    ))

(defn play-note [snd time pitch dur]
  (let [id (at time (pony (note pitch)))]
    (at (+ time dur) (ctl id :gate 0))
  ))

(let [play (partial play-note pony)]
  (do
    (play (now) :c2 300)
    (play (+ (now) (beat-ms 4 143)) :c2 300)
    (play (+ (now) (beat-ms 6 143)) :g1 800)
    (play (+ (now) (beat-ms 8 143)) :f1 300)
    (play (+ (now) (beat-ms 12 143)) :f1 300)
    (play (+ (now) (beat-ms 14 143)) :bb1 800)
    (play (+ (now) (beat-ms 16 143)) :c2 400)))
