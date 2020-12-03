#lang racket

(require (except-in srfi/1 span))
(require racket/match)
(require (only-in racket/function curry))
(require rebellion/collection/list)
(require rebellion/collection/multiset)
(require rebellion/collection/hash)
(require rebellion/streaming/transducer)
(require rebellion/type/wrapper)
(require rebellion/type/enum)
(require rebellion/base/option)
(require rebellion/type/singleton)

(define-singleton-type ⃒)
(define-wrapper-type ↥)
(define-wrapper-type ↧)

(define (count-in-mset v content)
  (- (multiset-frequency v (↥ content))
     (multiset-frequency v (↧ content))))

(define (keep? val combo)
  (eq? 1 (modulo (count-in-mset combo val) 2)))

(define (extract-value x) (if (↥? x) (↥-value x) (↧-value x)))

(define (add-aux m1 m2)
  (let* ([combo (multiset-add-all m1 m2)]
         [uniq-elems (set->list (multiset-unique-elements combo))]
         [new-elems (filter (lambda (e) (keep? e combo))
                            (remove-duplicates (map extract-value uniq-elems)))])
    (apply multiset (map ↥ new-elems))))

(define (add . msets)
  (fold add-aux empty-multiset msets))

(define (mult . msets)
  (define (mult-aux level x y)
    (match (cons x y)
      [(cons a b) #:when (equal? a b) a]
      [(cons a b) #:when (⃒? b) a]
      [(cons a b) #:when (⃒? a) b]
      [(cons (↥ a) (↥ b)) (↥ (mult-aux level a b))]
      [(cons (↧ a) (↥ b)) (↧ (mult-aux level a b))]
      [(cons (↥ a) (↧ b)) (↧ (mult-aux level a b))]
      [(cons (↧ a) (↧ b)) (↥ (mult-aux level a b))]
      [(cons (? multiset? m1) (? multiset? m2))
       (if (eq? 0 level)
           (for*/multiset ([a m1] [b m2]) (mult-aux 1 a b))
           (add m1 m2))]))
  (add (fold (curry mult-aux 0) ⃒ msets)))

(provide ⃒ ⃒? ↥ ↥? ↥-value ↧ ↧? ↧-value add mult)
