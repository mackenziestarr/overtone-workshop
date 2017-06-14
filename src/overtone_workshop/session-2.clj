(ns overtone-workshop.core
  (require [overtone.live :refer :all]))

;; Session 2 : Making our own instruments

;; load the shit
;; lein repl
;; (use 'overtone.live)

;; first, two essential functions
;; (odoc)
;; (stop)

;; In overtone, you can make an instrument with the macro `definst`
(definst my-inst []
  (sin-osc))

;; it looks kind of like a Clojure function
(defn example-add-1 [arg]
  (+ 1 arg))

;; but... it does some magic

;; it expects your implementation to return a signal that it can
;; routes to your sound card
(definst my-inst []
  (sin-osc))

(my-inst)
(stop)


;; arguments to the definst macro are also defined and treated specially
(definst my-inst [frequency 300]
  (sin-osc frequency))

;; falls back on default value
(my-inst)
(stop)

;; instantiate with new frequency value
(my-inst 600)
(stop)

;; but the argument values actually get transformed into magic things
;; we can rebind new values to later for great effect
(definst my-inst [frequency 300]
  (println frequency) ; #overtone.sc.machinery.ugen.sc_ugen.ControlProxy
  (sin-osc frequency))

(my-inst)
(ctl my-inst :frequency 600)
(ctl my-inst :frequency 1200)

;; so moving on... how do we use this

;; hello world
(definst my-inst []
  (sin-osc))
(my-inst)
(stop)


;; controlling the frequency
(definst frequency-example [freq 400]
  (sin-osc freq)
)
(frequency-example)
(ctl frequency-example :freq 200)
(ctl frequency-example :freq 440)
(stop)


;; controlling the volume
(definst amplitude-example [amp 0.2]
  (* amp (sin-osc 400))
)
(amplitude-example)
(ctl amplitude-example :amp 0)
(ctl amplitude-example :amp 1)
(ctl amplitude-example :amp 0.5)
(stop)

;; filtering with mouse-x
(definst my-filtered-saw [freq 440]
  (lpf (lf-saw) (mouse-x 40 2000 EXP)))
(my-filtered-saw)

;; dubstep - aka moving the filter frequency cutoff
(definst dubstep [lfo-freq 0.2]
  (let [cutoff (lin-exp (sin-osc lfo-freq) -1 1 100 5000)]
    (rlpf (lf-saw 100) cutoff 0.3)))
(dubstep)
(ctl dubstep :lfo-freq 5)
(ctl dubstep :lfo-freq 2)
(ctl dubstep :lfo-freq 0.5)
(stop)

;; sweet trick for thicking your sounds
(definst thick [freq 200]
  (mix (square (* 200 [1 0.99 1.01]))))
(thick)
(stop)

;; honestly sometimes i just do this because i think it sounds awesome
(definst weird-sine-ence [freq 200]
  (mix (sin-osc (* freq [1 0.8 1.3 1.77 2]))))
(weird-sine-ence 200)
(ctl weird-sine-ence :freq 300)
(ctl weird-sine-ence :freq 666)
(stop)

;; thick dubstep
(definst thick-dubstep [lfo-freq 0.2]
  (let [cutoff (lin-exp (sin-osc lfo-freq) -1 1 100 5000)
        snd (mix (lf-saw (* 100 [1 0.99 1.5 2])))]
    (rlpf snd cutoff 0.3)))
(thick-dubstep)
(ctl thick-dubstep :lfo-freq 5)
(ctl thick-dubstep :lfo-freq 2)
(ctl thick-dubstep :lfo-freq 0.5)
(stop)

;; it would be nice if the sound could stop
;; enter envelopes they automate amplitude over time
(definst showing-perc-envelope []
  (let [snd (lf-saw 400)
        env (env-gen (perc) FREE)]
    (* env snd)))
(showing-envelopes)

(definst showing-triangle-envelope []
  (let [snd (lf-saw 400)
        env (env-gen (triangle) FREE)]
    (* env snd)))
(showing-triangle-envelope)

(definst showing-sine-envelope []
  (let [snd (lf-saw 400)
        env (env-gen (lin) FREE)]
    (* env snd)))
(showing-sine-envelope)
;; *envelopes can also take arguments (@mstarr to show this)


;; putting it all together
;; every concept we learned about in one definst
(definst etsy-school-awesome-synth [freq 400 lfo-freq 10]
  (let [snd (lf-saw (* freq [0.99 1 1.01])) ;; thick saw wave
        cutoff (+ (* (sin-osc lfo-freq) 800) 1000) ;; cutoff freq range [200 1800]
        filter (lpf snd cutoff) ;; low-pass filter with sound and cutoff
        env (env-gen (perc 0.1 1) FREE) ;; drum-like envelope
        ]
    (* env filter)))

(etsy-school-awesome-synth)
;; hooray
(doseq [freq [200 400 500 700 100]]
  (etsy-school-awesome-synth freq)
  (Thread/sleep 500))


;; CHALLENGE TIME!

;; ex.1 write a definst that plays a sine wave
;; components:
;; - sin-osc

;; ex.2 write a definst that uses a saw wave and low-pass filter
;; components:
;; - lf-saw
;; - lpf

;; ex.3 write a definst that uses an envelope
;; components:
;; - an oscillator (lf-saw, sin-osc, square)
;; - an envelope e.g. (env-gen (perc) FREE)

;; ex.4 (bonus) write your own synth using the following
;; components
;; - a rich oscillator (lf-saw, square)
;; - an envelope (lin, triangle, perc) e.g (env-gen (lin) FREE)
;; - a low-pass filter (lpf)


;; probably ignore this
;; some more stuff you can play with later and ask about in #clojure
(definst random-pitch-sine [foobar 4]
  (let [freq (lin-exp (lf-noise0 foobar) -1 1 200 1000)
        amp  (lin-exp (lf-noise1 12) -1 1 0.02 1)]
    (* amp (sin-osc freq))))

(random-pitch-sine)
(stop)

(definst pulse-rand [ampHz 2 fund 400 maxPartial 8 width 0.5]
  (let [amp1 (* 0.75 (lf-pulse:kr ampHz 0 0.12))
        amp2 (* 0.75 (lf-pulse:kr ampHz 0.5 0.12))
        freq1 (round (lin-exp (lf-noise0:kr 4) -1 1 fund (* maxPartial fund)) 1)
        freq2 (round (lin-exp (lf-noise0:kr 4) -1 1 fund (* maxPartial fund)) 1)
        sig1 (free-verb (* amp1 (pulse freq1 width)) 0.7 0.8 0.25)
        sig2 (free-verb (* amp2 (pulse freq2 width)) 0.7 0.8 0.25)
        ]
    (mix [sig1 sig2])))

(pulse-test)
(stop)

(definst pony [note 36 gate 1]
  (let [amp (env-gen (asr 0.01 1 0.5) gate :action FREE)
        snd (var-saw (midicps note) 0 0.95)
        filter-frequency (lin-exp (env-gen (sine 0.8)) 0 1 200 800)
        filter (rlpf snd filter-frequency 0.2)
        sample-and-hold (latch:ar filter (sin-osc 2000))]
    (* amp sample-and-hold)))

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
