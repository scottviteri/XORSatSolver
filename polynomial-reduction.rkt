#lang racket

(require srfi/1)
(require rebellion/collection/list)
(require rebellion/collection/multiset)
(require rebellion/collection/hash)
(require rebellion/streaming/transducer)
(require rebellion/type/wrapper)
(require rebellion/type/enum)
(require rebellion/base/option)
(require rebellion/type/singleton)
(require brag)
(require brag/support)
(require "poly.rkt")


;;;;;; Msets ;;;;;;
(define-singleton-type ⃒)
(define-wrapper-type ↥)
(define-wrapper-type ↧)

(define (int? x)
  ; (int? (make-mset 2)) -> #t
  (and (multiset? x) (andmap (lambda (x) (or (equal? x (↥ ⃒)) (equal? x (↧ ⃒))))
                             (multiset->list x))))

(define (add . msets)
  ;(read-mset (add (make-mset -2) (make-mset 3))) ; 1
  ;consider normalizing here
  (fold multiset-add-all empty-multiset msets))

(define (mult . msets)
  (define (mult-aux level x y)
    (match (cons x y)
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
; [(cons (? multiset? m1) b)
;  (if (eq? 0 level) (for*/multiset ([a m1]) (mult-aux 1 a b )) (add m1 b))]
; [(cons a (? multiset? m2))
;  (if (eq? 0 level) (for*/multiset ([b m2]) (mult-aux 1 a b)) (add a m2))]
  (fold (curry mult-aux 0) ⃒ msets))

(define (make-mset x)
  (if (number? x)
    (multiset-set-frequency empty-multiset (if (>= x 0) (↥ ⃒) (↧ ⃒)) (abs x))
    (if (string? x)
        (let* ([pieces (string-split x "^")]
               [var-index (- (char->integer (car (string->list (car pieces)))) 97)]
               [power (if (eq? (length pieces) 1) 1 (string->number (cadr pieces)))])
          (make-mset `((((,var-index . ,power)) . 1))))
        (apply add (map (lambda (p) (mult (multiset (↥ (make-mset (car p))))
                                     (make-mset (cdr p)))) x)))))

(define (read-mset m)
  ; (compose read-mset make-mset) ~ id
  (define (count-in-mset v content)
    (- (multiset-frequency v (↥ content))
       (multiset-frequency v (↧ content))))
  (define (extract-value x) (if (↥? x) (↥-value x) (↧-value x)))
  (define (read-mset-aux m)
    (if (int? m)
        (count-in-mset m ⃒)
        (let ([uniq-vals (remove-duplicates (map extract-value (multiset->list m)))])
          (map (lambda (p) (cons (read-mset-aux p) (count-in-mset m p))) uniq-vals))))
  (read-mset-aux m))

;;;;;;;; Mset utilities ;;;;;;;;;;

(define (view-mset-as-polynumber m)
  ;(view-mset-as-polynumber (make-mset '((-1 . 3) (2 . 2))))
  ;(list 3 (z 0) 0 2)
  (define-wrapper-type z)
  (let* ([powers-to-coeffs (read-mset m)]
         [powers (dict-keys powers-to-coeffs)]
         [min_power (apply min powers)]
         [max_power (apply max powers)])
    (map (lambda (i) (let ([val (dict-ref powers-to-coeffs i 0)])
                  (if (eq? i 0) (z val) val)))
         (range (min 0 min_power) (max 1 (+ 1 max_power))))))

(define (view-mset-as-polynomial v)
  ;(view-mset-as-polynomial (make-mset '((-1 . 3) (2 . 2))))
  ;"3 * x^-1 + 2 * x^2"
  (let* ([idx-poly (sort (read-mset v)
                         (lambda (x y) (< (car x) (car y))))]
         [string-terms (filter-map (lambda (p) (if (eq? (cdr p) 0) #f
                                              (~a (cdr p) " * x^" (car p))))
                            idx-poly)])
    (string-join
     (map string->immutable-string string-terms)
     " + ")))

(define (view-mset-as-multinomial m)
  ;(view-mset-as-multinomial (make-mset '((((0 . 1) (1 . 3)) . 2) (((1 . 1)) . 1))))
  ;"b + 2ab^3"
  (define (get-degree assoc-lst) (apply + (map cdr assoc-lst)))
  (define (index-to-var i) (integer->char (+ 97 i)))
  (define (power-to-string index-power) ;((0 . 1) (1 . 3)) -> 2ab^3
    (string-join
     (map (lambda (p) (if (eq? 1 (cdr p))
                     (~a (index-to-var (car p)))
                     (~a (index-to-var (car p)) "^" (cdr p))))
          index-power)
     ""))
  (let ([idx-poly (sort (read-mset m)
                        (lambda (x y) (< (get-degree (car x)) (get-degree (car y)))))])
    (string-join (map (lambda (p) (if (eq? 1 (cdr p))
                                 (power-to-string (car p))
                                 (string-append (number->string (cdr p)) (power-to-string (car p)))))
                      idx-poly)
                 " + ")))

;; should I create string to mset?
;; a * a <-- parse each parse into multiset then multiply

;;;;; Convert Mset Base ;;;;;;;;

(define (convert-to-base base input)
  ;(convert-to-base 2 '(8)) -> '(0 0 0 1)
  ;(convert-to-base 2 '(1 3 1 5 6)) -> '(1 1 0 0 1 0 0 1)
  (define (convert-to-base-aux output input)
    (if (null? input) output
        (let*-values ([(next-val) (car input)]
                      [(q r) (quotient/remainder next-val base)])
          (let ([next-output (cons r output)]
                [next-input (if (null? (cdr input))
                                (if (= q 0) '() (list q))
                                (cons (+ q (cadr input)) (cddr input)))])
            (convert-to-base-aux next-output next-input)))))
  (reverse (convert-to-base-aux '() input)))

(define (sparse-convert-to-base base assoc-list)
  ;'((8 . 0)) -> '((1 . 3))
  ;'((1 . 0) (3 . 1) (1 . 2) (5 . 3) (6 . 4))
  ; -> '((1 . 0) (1 . 1) (1 . 4) (1 . 7))
  (define (conditional-add dict ind val)
    (if (dict-ref dict ind #f)
        (dict-set dict ind (+ val (dict-ref dict ind)))
        (dict-set dict ind val)))
  (define (sparse-convert-aux output input)
    (if (null? input) output
        (let*-values ([(next-val) (car input)]
                      [(power coeff) (car+cdr next-val)]
                      [(q r) (quotient/remainder coeff base)])
          (if (= 0 coeff) (sparse-convert-aux output (cdr input))
              (let* ([next-input
                      (conditional-add (cdr input) (+ 1 power) q)]
                     [next-output (if (= r 0) output (cons (cons power r) output))])
                (sparse-convert-aux next-output next-input))))))
  (reverse
   (sparse-convert-aux '() (sort assoc-list (lambda (x y) (< (car x) (car y)))))))

(define (mset-convert-to-base base mset)
  ;(read-mset (mset-convert-to-base 2 (make-mset '((-1 . 3) (2 . -2)))))
  ;((1 . -1) (-1 . 3))
  (make-mset (sparse-convert-to-base base (read-mset mset))))
; unsure how to convert to base in multinomial setting?

;;;;;; Mset coeff field ;;;;;;

(define (restrict-mset field-size mset)
  ;(read-mset (restrict-mset 2 (make-mset '((-1 . 3) (2 . -2))))) -> '((-1 . 1))
  (make-mset (map (lambda (p) (cons (car p) (modulo (cdr p) field-size)))
                   (read-mset mset))))
; will have to redefine add and mult to take field into account natively

;;;;; Front end ;;;;;;;;

(define (tokenize s)
  (for/list ([str (regexp-match* #px"\\(|\\)|-?[0-9]+|[a-z]|\\+|\\*|\\^" s)])
    (match str
      ["(" (token 'LEFT-PAREN str)]
      [")" (token 'RIGHT-PAREN str)]
      ["+" (token 'PLUS str)]
      ["*" (token 'TIMES str)]
      ["^" (token 'EXP str)]
      [else (if (string->number str)
                (token 'NUM (string->number str))
                (token 'VAR str))])))

(define (evaluate datum)
  (match datum
    [(list (quote plus_expr) x) (evaluate x)]
    [(list (quote mult_expr) x) (evaluate x)]
    [(list (quote variable) x) (evaluate x)]
    [(list (quote variable) x pow)
     (make-mset (string-append x "^" (number->string pow)))]
    [(cons (quote plus_expr) lst) (apply add (map evaluate lst))]
    [(cons (quote mult_expr) lst) (apply mult (map evaluate lst))]
    [else (mult (make-mset datum)
                (if (number? datum) (make-mset '((0 . 1))) ⃒))]))

(define interpret (compose evaluate syntax->datum parse tokenize))


(provide (all-defined-out))