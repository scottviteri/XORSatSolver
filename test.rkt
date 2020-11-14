#lang racket

(require "polynomial-reduction.rkt")

(require srfi/1)
(require rebellion/collection/list)
(require rebellion/collection/multiset)
(require rebellion/collection/hash)
(require rebellion/streaming/transducer)
(require rebellion/type/wrapper)
(require rebellion/type/enum)
(require rebellion/base/option)
(require rebellion/type/singleton)

(define neg-one (multiset (↧ ⃒)))
(define zero (multiset))
(define one (multiset (↥ ⃒)))
(define two (multiset (↥ ⃒) (↥ ⃒)))
;(mult (↥ ⃒) (↧ ⃒) (↧ ⃒) (↧ ⃒)); -> (↥ #<⃒>)
(read-mset (mult (make-mset -3) (make-mset 2))); -> -6

 ;'(power . coeff)
(read-mset (mult (make-mset '((1 . 2))) (make-mset 1)))
; '((1 . 2))
(read-mset (mult (make-mset '((1 . 2))) (make-mset 2)))
;'((1 . 4))
(read-mset (mult (make-mset '((2 . 1))) (make-mset '((2 . 1)))))
;'((4 . 1))

; 3*x^-1 - 2x^2
;(make-mset '((-1 . 3) (2 . -2)))

; ((index . power) . coeff)
(make-mset '((((0 . 1) (1 . 3)) . 2) (((1 . 1)) . 1)))
; b + 2ab^3

(define x (make-mset '((1 . 1)))) ;(multiset (↥ (multiset (↥ #<⃒>))))
(define x^2 (make-mset '((2 . 1)))) ;(multiset (↥ (multiset (↥ #<⃒>) (↥ #<⃒>))))

; explain format by imagining sparse repr
;  dictionary from power to coeff (reason for mset order)
;  multimset order also because of uniqueness

(define a (make-mset (map cons (make-list 30 1) (map (curry * 10) (iota 30)))))
;(time (void (mult a a)))
; sparse reps work well

(view-mset-as-polynomial (make-mset '((1 . -1)))) ;"-1 * x^1"
(view-mset-as-polynomial
 (mult (make-mset '((1 . -1))) (make-mset '((1 . 1)))))
;"-1 * x^2"
(view-mset-as-polynomial
 (mult (make-mset '((-1 . 1))) (make-mset '((1 . 1)))))
;"1 * x^0"

;;; demo ;;;

(make-mset 3)
(make-mset -2)
(add (make-mset 3) (make-mset -2))
(mult (make-mset 3) (make-mset -2))

(make-mset '((1 . 1)))
(make-mset '((-1 . 3) (2 . -2)))

(read-mset
 (add
  (make-mset '((1 . 1)))
  (make-mset '((1 . 1)))))
(read-mset
 (mult
  (make-mset '((1 . 1)))
  (make-mset '((1 . 1)))))

(mult
 (multiset (↥ (multiset (↥ two))))
 (multiset (↥ (multiset (↥ two))))) ; [[2]] * [[2]] = y * y

;2xy^3 * y -> 2xy^4
;[[1 2 2 2] [1 2 2 2]] * [[2]] -> [ [1 2 2 2 2] [1 2 2 2 2]]
;(mult (multiset (↥ (multiset (↥ one) (↥ two) (↥ two) (↥ two)))
;                (↥ (multiset (↥ one) (↥ two) (↥ two) (↥ two))))
;      (multiset (↥ (multiset (↥ (multiset (↥ #<⃒>) (↥ #<⃒>)))))))
;(multiset (↥ (multiset (↥ one) (↥ two) (↥ two) (↥ two) (↥ two)))
;          (↥ (multiset (↥ one) (↥ two) (↥ two) (↥ two) (↥ two))))
; falls out of same add and mult rules!
;(mult
; (multiset (↥ (multiset (↥ two))))
; (multiset (↥ (multiset (↥ two))))) [[2]] * [[2]] = y * y
; [[2 2]] <-- y^2
;(add
; (multiset (↥ (multiset (↥ two))))
; (multiset (↥ (multiset (↥ two))))) ; y + y
;(multiset
; (↥ (multiset (↥ two)))
; (↥ (multiset (↥ two)))) ; 2 * y
;
;lation
;(make-mset '((1 . 1))) ; (multiset (↥ (multiset (↥ #<⃒>))))
;(make-mset '((((0 . 1)) . 1))) ; (multiset (↥ (multiset (↥ (multiset)))))
;
;(read-mset (exp (make-mset "a") 2))

(read-mset (interpret "-1 + 2 * b + a"))
;'((((1 . 1)) . 2) (((0 . 1)) . 1) (0 . -1))
(read-mset (interpret "1 + 2*b^3 + -1*a"))
;'((((1 . 3)) . 2) (((0 . 1)) . -1) (0 . 1))
(read-mset (interpret "-1*a*c^3*d"))
;'((((3 . 1) (0 . 1) (2 . 3)) . -1))
