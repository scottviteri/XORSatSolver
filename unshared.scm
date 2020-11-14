(use-modules
 (srfi srfi-1)
 (srfi srfi-69)
 (ice-9 match))

(define (can-apply-comm? x)
  (and (list? x) (eq? (car x) '*) (symbol? (cadr x)) (symbol? (caddr x))))
(define (symbol<=? s1 s2)
  (string<=? (symbol->string s1) (symbol->string s2)))

; use symbol 1 not number 1
(define (poly-reduction p) ; push down mults to leaves
  (match p
    [('* ('+ a b) c)
     (poly-reduction (list '+ (list '* a c) (list '* b c)))]
    [('* a ('+ b c))
     (poly-reduction (list '+ (list '* a b) (list '* a c)))]
    [(? can-apply-comm? x) ; only using on symbols
     (let ([a (cadr x)] [b (caddr x)])
       (if (symbol<=? a b)
           (list '* (poly-reduction a) (poly-reduction b))
           (list '* (poly-reduction b) (poly-reduction a))))]
    [('+ a b) (list '+ (poly-reduction a) (poly-reduction b))] ; maybe assoc
    [else p]))

(poly-reduction '(* (+ x0 x1) (+ x0 x1)))
;(+ (+ (* x0 x0) (* x0 x1)) (+ (* x0 x1) (* x1 x1)))
; then could write function to coeffs, but not uniform representation
;  also inefficient to produce and count

; an expression tree is a bad datastructure for a polynomial
; because it does not internalize its equational laws
