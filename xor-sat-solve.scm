(use-modules
 (srfi srfi-1)
 (srfi srfi-69)
 (ice-9 match)
 (ice-9 popen)
 (ice-9 rdelim)
 (ice-9 receive)
 (ice-9 vlist))

; create hash-cons repr
(define-syntax test
  (syntax-rules ()
    ((_ commands ...)
     (begin
       (display (begin (reset)
                       commands
                       ...))
       (newline)))))
(define (pa f x) (lambda (y) (f x y)))


(define H (alist->vhash '((x0 . 2) (one . 1) (zero . 0))))
(define A (list->vlist '(x0 one zero)))
(define memo (alist->vhash '()))

(define (reset)
  (set! H (alist->vhash '((x0 . 2) (one . 1) (zero . 0))))
  (set! A (list->vlist '(x0 one zero)))
  (set! memo (alist->vhash '())))

(define* (lookup term)
   (if (number? term) term
       (let ((found (vhash-assoc term H)))
         (if found
             (cdr found)
             (let ((new-id (vlist-length A)))
               (set! H (vhash-cons term new-id H))
               (set! A (vlist-cons term A))
               new-id)))))

(test
  (lookup '(* 0 1))
  (vhash-assoc '(* 0 1) H)); -> ((* 0 1) . 3)


(define (s-and t1 t2)
 (lookup (list '* t1 t2)))
(define (s-xor t1 t2)
 (lookup (list '+ t1 t2)))

(define leaf? (compose not list?))

(define (share term)
  ; (share '(+ (* x0 x1) (+ x0 x2)))
  (if (leaf? term)
      (lookup term)
      (if (eq? (car term) '*)
          (s-and (share (cadr term))
                 (share (caddr term)))
          (s-xor (share (cadr term))
                 (share (caddr term))))))

(define (vlist-ref' index)
  (vlist-ref A (- (vlist-length A) index 1)))

(define (unroll prog-idx)
  (let ((prog (vlist-ref' prog-idx)))
    (if (leaf? prog) prog
        (list (car prog)
              (unroll (cadr prog))
              (unroll (caddr prog))))))

(test
  (lookup 'x1)
  (lookup (list '* 2 3))
  (lookup (list '+ 4 4))
  (unroll 5)) ; (+ (* x0 x1) (* x0 x1))

(define (compile-to-xor formula) ; (∧ (∧ x0 x0) x1) -> 6
  (if (leaf? formula) (lookup formula)
      (if (= 2 (length formula)) ; ¬ case
          (share (list '+ 'one (compile-to-xor (cadr formula))))
          (let* ([binop (car formula)]
                 [fst (cadr formula)]
                 [snd (caddr formula)]
                 [fst-xor (compile-to-xor fst)]
                 [snd-xor (compile-to-xor snd)])
            (share (match binop
                     ['∧ (list '* fst-xor snd-xor)]
                     ['∨ (list '+ fst-xor (list '+ snd-xor (list '* fst-xor snd-xor)))]
                     ['⇒ (list '+ 'one (list '+ fst-xor (list '* fst-xor snd-xor)))]
                     ['⊕ (list '+ fst-xor snd-xor)]
                     ['⇔ (list '+ 'one (list '+ fst-xor snd-xor))]))))))


(test
 (compile-to-xor '(⊕ (∧ x0 x1) (∧ x0 x1))) ;5
 (display (vlist->list H)) (newline)
 ;(((+ 4 4) . 5) ((* 2 3) . 4) (x1 . 3) (x0 . 2) (one . 1) (zero . 0))
 (unroll 5)) ; (+ (* x0 x1) (* x0 x1))

(test (unroll (compile-to-xor '(⇒ (⇔ x0 one) (⇔ x0 one)))))
; (+ one (+ (+ one (+ x0 one)) (* (+ one (+ x0 one)) (+ one (+ x0 one)))))

; A + B <-> B + A ; can implement by moving to normal form (using hash order)
; A * B <-> B * A ; can implement by moving to normal form
; A + A -> 0
; A * A -> A
; A + 0 -> A
; A * 1 -> A
; A * 0 -> 0
; A * (B + C) -> A * B + A * C ; could do greedily
; (A + B) * C -> A * C + B * C ; could do greedily
; greedy distrib law is same as pushing multiplications down in the tree
; also evaluation order matters (want to do push mult down last)
; be careful with distrib <-- could split to prevent space blowup

; don't forget assoc of add and mult <-- maybe not necessary?
; what about (+ 1 (+ 1 x0))? want to reduce to x0
; this is why not doing in one step

; A + A -> 0
; A * A -> A
; A + 0 -> A
; 0 + A -> A
; A * 1 -> A
; 1 * A -> A
; A * 0 -> 0
; 0 * A -> 0

(define (idemp-add? lst) (and (list? lst) (eq? (car lst) '+) (eq? (cadr lst) (caddr lst))))
(define (idemp-mult? lst) (and (list? lst) (eq? (car lst) '*) (eq? (cadr lst) (caddr lst))))
(define (mult-distrib-fst-lst? lst) (and (eq? (car lst) '*) ;vref
                                         (not (leaf? (cadr lst)))
                                         (eq? (car (cadr lst)) '+)))
(define (mult-distrib-snd-lst? lst) (and (eq? (car lst) '*)
                                         (not (leaf? (caddr lst)))
                                         (eq? (car (caddr lst)) '+)))
(define (simplify-xor prog-idx) ; currently caches intermediate computations
  (let ([formula (vlist-ref' prog-idx)])
    (if (leaf? formula) prog-idx ; no nots
        (match formula
          [('+ '0 snd) (simplify-xor snd)]
          [('+ fst '0) (simplify-xor fst)]
          [('* '0 snd) (lookup 'zero)]
          [('* fst '0) (lookup 'zero)]
          [('* '1 snd) (simplify-xor snd)]
          [('* fst '1) (simplify-xor fst)]
          [(? idemp-add? x) (lookup 'zero)]
          [(? idemp-mult? x) (simplify-xor (cadr x))]
          [('+ fst snd) (lookup (if (<= fst snd)
                                    (list '+ (simplify-xor fst) (simplify-xor snd))
                                    (list '+ (simplify-xor snd) (simplify-xor fst))))]
          [(? mult-distrib-snd-lst? x) ; (* a (+ b c)) -> (+ (* a b) (* a c))
           (let* ([a (cadr x)] [b (cadr (caddr x))] [c (caddr (caddr x))]
                  [sa (simplify-xor a)] [sb (simplify-xor b)] [sc (simplify-xor c)])
             (share (list '+ (list '* sa sb) (list '* sa sc))))]
          [(? mult-distrib-snd-lst? x) ; (* (+ a b) c) -> (+ (* a c) (* b c))
           (let* ([a (cadr (cadr x))] [b (caddr (cadr x))] [c (caddr x)]
                  [sa (simplify-xor a)] [sb (simplify-xor b)] [sc (simplify-xor c)])
             (share (list '+ (list '* sa sc) (list '* sb sc))))]
          [('* fst snd) (lookup (list '* (simplify-xor fst) (simplify-xor snd)))]
          [else formula]))))

; final form 1 + a + b + c + ab + ac + bc + abc <-- if all coeffs one
; will have to do fixpoint, because for example (* (+ a b) (+ c d))
; would have to extend vhash to be nary, maybe not terrible <-- worth doing in other file


; want to make n-ary, by collecting by assoc
; normal commutative form not enough (+ 1 (+ x0 (+ 1 x0)))

(test (unroll (simplify-xor (share '(+ zero x1))))) ;x1
(test (unroll (simplify-xor (share '(* x1 zero))))) ;zero
(test (unroll (simplify-xor (share '(* zero x1))))) ;zero
(test (unroll (simplify-xor (share '(* x1 x1)))))   ;x1
(test (unroll (simplify-xor (share '(+ x1 x1)))))   ;zero
(test (unroll (simplify-xor (share '(* (+ x1 x2) (+ x3 x4))))))
(test (unroll (simplify-xor (share '(+ (+ x1 x2) (+ x1 x2)))))) ; zero
(test (unroll (simplify-xor (share '(+ one (+ one x0)))))) ; (+ one (+ one x0))
;(test (simplify-xor (share '(+ one (+ (+ one (+ x0 one))
;                                      (* (+ one (+ x0 one)) (+ one (+ x0 one))))))))
