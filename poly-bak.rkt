(require srfi/1)
(require rebellion/collection/list)
(require rebellion/collection/multiset)
(require rebellion/collection/hash)
(require rebellion/streaming/reducer)
(require rebellion/streaming/transducer)
(require rebellion/type/wrapper)
(require rebellion/type/enum)
(require rebellion/base/option)
(require rebellion/type/singleton)

;;;;;; primitive mark/antimarks ;;;;;;
(define-singleton-type ⃒)
(define-wrapper-type ↥)
(define-wrapper-type ↧)
(define one (multiset (↥ ⃒)))
(define two (multiset (↥ ⃒) (↥ ⃒)))
(define neg-one (multiset (↧ ⃒)))
;(define-enum-type sign (↑ ↓))
;(define (prim* x y) (if (eq? x y) ↑ ↓))
(define (prim* . signs)
  (if (eq? 0 (modulo (count (curry eq? ↓) signs) 2)) ↑ ↓))

;;;;;; integers ;;;;;;;;

(define (make-int n) ;(make-int 2) -> (multiset #<sign:↑> #<sign:↑>)
  (multiset-set-frequency
   empty-multiset
   (if (>= n 0) ↑ ↓)
   (abs n)))
(define (int? x) ; (int? (make-int 2)) -> #t
  (and (multiset? x)
       (or (= 0 (multiset-size x))
           (multiset-contains? x ↑)
           (multiset-contains? x ↓))))
(define (int->num i) ; (compose int->num make-int) ~ identity
  (- (multiset-frequency i ↑)
     (multiset-frequency i ↓)))
(define (int+ . ints)
  (fold multiset-add-all empty-multiset ints))
;(define int+ multiset-add-all)

; nary int+ and int*
(define (int* x y) (for*/multiset ([a x] [b y]) (prim* a b)))
(define (normalize-int i)
  ;(int->num (normalize-int (int+ (make-int 3) (make-int -2)))) -> 1
  (let ([pos-count (multiset-frequency i ↑)]
        [neg-count (multiset-frequency i ↓)])
    (cond
     [(eq? pos-count neg-count)
      (multiset-set-frequency
       (multiset-set-frequency i ↑ 0)
       ↓ 0)]
     [(> pos-count neg-count)
      (multiset-set-frequency
       (multiset-set-frequency i ↑ (- pos-count neg-count))
       ↓ 0)]
     [else
      (multiset-set-frequency
       (multiset-set-frequency i ↑ 0)
       ↓ (- neg-count pos-count))])))


;;;;;; singletons ;;;;;;

(define-wrapper-type sng)
(define (sng* . sngs)
  (sng (apply int+ (map sng-value sngs))))
;(sng* (sng (make-int 3)) (sng (make-int 2)))
;(sng (multiset #<sign:↑> #<sign:↑> #<sign:↑> #<sign:↑> #<sign:↑>))

;;;;;; vexels ;;;;;;

(define (make-vexel . nums)
  (apply multiset (map (compose sng make-int) nums)))
;(make-vexel 1 1 3) ->
;(multiset
; (sng (multiset #<sign:↑> #<sign:↑> #<sign:↑>))
; (sng (multiset #<sign:↑>))
; (sng (multiset #<sign:↑>)))

;(define (vexel* . vexs)
;  (define (vexel*-aux v1 v2)
;    (for*/multiset ([x v1] [y v2]) (sng* x y)))
;  (fold vexel*-aux (make-vexel 0) vexs))
(define (vexel* . vexs)
  (let ([prods (apply cartesian-product (map multiset->list vexs))])
    (apply multiset (map (curry apply sng*) prods))))

;(vexel* (make-vexel -1) (make-vexel 1) (make-vexel 1 1 3))
;(multiset
; (sng (multiset #<sign:↓> #<sign:↑> #<sign:↑> #<sign:↑> #<sign:↑>))
; (sng (multiset #<sign:↓> #<sign:↑> #<sign:↑>))
; (sng (multiset #<sign:↓> #<sign:↑> #<sign:↑>)))
;add vexel normalize

(define x (make-vexel 1))
;(multiset (sng (multiset #<sign:↑>)))
(define x^2 (vexel* x x))
;(multiset (sng (multiset #<sign:↑> #<sign:↑>)))

(define (vexel^ m n) ; better to define recursively with vexel*
  (fold vexel* (make-vexel 0) (make-list (int->num n) m)))
;(vexel^ x (make-int 3))
;(multiset (sng (multiset #<sign:↑> #<sign:↑> #<sign:↑>)))

;;;;;;;; Vexel utilities ;;;;;;;;;;

(define (poly assoc-lst) ; (power . coefficient)
  (let ([args (apply append (map (lambda (p) (make-list (cdr p) (car p)))
                                 assoc-lst))])
    (apply make-vexel args)))
;(poly '((3 . 1) (2 . 2)))
;(multiset
;    (sng (multiset #<sign:↑> #<sign:↑>))
;    (sng (multiset #<sign:↑> #<sign:↑>))
;    (sng (multiset #<sign:↑> #<sign:↑> #<sign:↑>)))

(define (view-vexel-as-polynumber v)
  (let* ([assoc-list (vexel->assoc-list v)]
         [keys (dict-keys assoc-list)]
         [min_k (apply min keys)]
         [max_k (apply max keys)])
    (string-join
     (map (lambda (i) (let ([val (number->string (dict-ref assoc-list i 0))])
                   (if (eq? i 0) (string-append "_" val "_") val)))
          (range (min 0 min_k) (max 1 (+ 1 max_k)))))))

;(view-vexel-as-polynumber (poly '((3 . 1) (2 . 2))))
;"_0_ 0 2 1"
;(view-vexel-as-polynumber (poly '((-3 . 1) (2 . 2))))
;"1 0 0 _0_ 0 2"

(define (view-vexel-as-polynomial v)
  (let* ([idx-poly (vexel->assoc-list v)]
         [string-terms (filter-map (lambda (p) (if (eq? (cdr p) 0) #f
                                              (~a (cdr p) " * x^" (car p))))
                            idx-poly)])
    (string-join
     (map string->immutable-string string-terms)
     " + ")))
;(view-vexel-as-polynomial (poly '((3 . 1) (2 . 2))))
;"2 * x^2 + 1 * x^3"
;(view-vexel-as-polynomial (poly '((-3 . 1) (2 . 2))))
;"2 * x^2 + 1 * x^-3"

;(vexel->polystring
; (vexel* (poly '((3 . 1) (2 . 2))) (poly '((3 . 1) (2 . 2)))))
;"4 * x^4 + 4 * x^5 + 1 * x^6"

; normalize vexel? also what to do when negative in make-vexel

;(define a (poly (map cons (map (curry * 10) (iota 100)) (make-list 100 1))))
;(time (void (vexel* a a))) ; sparse reps work well
;cpu time: 988 real time: 990 gc time: 8

(define (normalize-vexel v) 1)
;; may not need -- rather optimize inside of vexel* and vexel+

;; other tests
;(view-vexel-as-polynomial (poly '((-1 . 1))))
;"1 * x^-1"
;(view-vexel-as-polynomial (vexel* (poly '((-1 . 1))) (poly '((1 . 1)))))
;"1 * x^0"

;(view-vexel-as-polynomial (vexel* (poly '((1 . -1))) (poly '((1 . 1)))))
;(poly '((1 . -1)))
;(make-list -1 1)
; errors -- issue with negative coeff

; need to fix poly, and representation
; currently cannot represent -2*x (or neg coeffs at all)
; to fix, need to change sng into a sign itself, so can express a neg count
