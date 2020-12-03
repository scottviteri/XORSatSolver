#lang racket

(require "boolean-multinomial-reduction.rkt")
(require "multinomial-reduction-utils.rkt")
(require "sat-solver.rkt")
(require "test-utils.rkt")
(require plot)

(view-multinomial (compile-to-xor '(⇒ (∧ v0 v1) (∧ v0 v1))))
;"1"
(view-multinomial (compile-to-xor '(⇒ (∧ v0 v1) (¬ (∧ v0 v1)))))
;"1 + v0*v1"
(view-multinomial (compile-to-xor '(∧ (∧ v0 v1) (¬ (∧ v0 v1)))))
;"0"
(view-multinomial (compile-to-xor '(∧ v100 v100)))
;"v100"
(view-multinomial (compile-to-xor '(⇒ v100 v100)))
;"1"
(view-multinomial (compile-to-xor '(⇔ v1 v2)))
;"1 + v1 + v2"

(view-multinomial (compile-to-xor (generate-formula .9 .8 3)))

(define formulas (map (lambda (x) (generate-formula .9 .8 5)) (range 20)))
(define times (map (curry time-milli compile-to-xor) formulas))
(define num-vars (map count-unique-vars formulas))
(define tree-sizes (map tree-size formulas))

;(plot-new-window? #f)
;(plot-file (points (map vector tree-sizes times))
;           "formula-tree-size-v-time.png"
;           #:x-label "formula tree size" #:y-label "time (ms)")
;(plot-file (points (map vector num-vars times))
;      "num-distinct-variables-v-time.png"
;      #:x-label "formula's number of distinct variables v time (ms)" #:y-label "time (ms)")
