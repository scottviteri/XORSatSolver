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
     (display (begin (reset)
                     commands
                     ...)))))
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
  (lookup '(* 0 1 1))
  (vhash-assoc '(* 0 1 1) H)); -> ((* 0 1 1) . 3)

(define leaf? (compose not list?))

(define (vlist-ref' index)
  (vlist-ref A (- (vlist-length A) index 1)))

(define (unroll prog-idx)
  (let ((prog (vlist-ref' prog-idx)))
    (if (leaf? prog) prog
        (cons (car prog)
              (map unroll (cdr prog))))))

(test
  (lookup 'x1)
  (lookup (list '* 2 3 1))
  (lookup (list '+ 4 4))
  (unroll 5)) ; (+ (* x0 x1 one) (* x0 x1 one))

(define (share term)
  ; (unroll (share '(+ (* x0 x1 x2) (+ x0 x2)))) ;(+ (* x0 x1 x2) (+ x0 x2))
  (lookup (if (leaf? term)
              term
              (cons (car term) (map share (cdr term))))))

; consider taking n-ary input <-- could even simplify at that stage
; may not want to do n-ary input because or's blow up
; derive simplifications by hand
; (∧ (∧ x0 x0 x0) x1) -> 6
(define (compile-to-xor formula)
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
                     ['∨ (list '+ fst-xor snd-xor (list '* fst-xor snd-xor))]
                     ['⇒ (list '+ 'one fst-xor (list '* fst-xor snd-xor))]
                     ['⊕ (list '+ fst-xor snd-xor)]
                     ['⇔ (list '+ 'one fst-xor snd-xor)]))))))


(test
 (compile-to-xor '(⇒ (∧ x0 x1) (∧ x0 x1))) ; 5 ; only list len = 2 for now
 (display (vlist->list H)) (newline)
 ;(((+ 4 4) . 5) ((* 2 3) . 4) (x1 . 3) (x0 . 2) (one . 1) (zero . 0))
 (unroll 5)) ; (+ (* x0 x1) (* x0 x1))

(test (unroll (compile-to-xor '(⇒ (⇔ x0 one) (⇔ x0 one)))))
; (+ one (+ (+ one (+ x0 one)) (* (+ one (+ x0 one)) (+ one (+ x0 one)))))
; (+ one (+ one x0 one) (* (+ one x0 one) (+ one x0 one)))

;leaving out assoc
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

; A + B + C <-> Any permutation
; A * B * C <-> Any permutation
; A + ... + A -> if even 0 A
; A * ... * A -> A
; e1 + 0 + e2 -> e1 + e2
; e1 * 0 * e2 -> 0
; distrib of mult over add

(define (idemp-add? lst) (and (list? lst) (eq? (car lst) '+) (eq? (cadr lst) (caddr lst))))
(define (idemp-mult? lst) (and (list? lst) (eq? (car lst) '*) (eq? (cadr lst) (caddr lst))))

(define (remove-doubles lst)
; (remove-doubles '(1 1 2 2 2)) ; (2)
; (remove-doubles '(1 1 2 2 2 2)) ; ()
  (let ([unique-elems (delete-duplicates lst)])
    (delete-duplicates (filter-map (lambda (ue) (let ([num-occurences (count (pa = ue) lst)])
                                             (if (= 0 (modulo num-occurences 2)) #f ue)))
                                   lst))))

; term sharing may be better if we do normal form
; sort by lookup

(define (check-op op prog-idx)
  (let ([formula (vlist-ref' prog-idx)])
    (if (leaf? formula) #f (eq? op (car formula)))))
;(define (merge-top-level formula)
;  (let* ([op (car formula)]
;         [rest (cdr formula)]
;         [lift-terms (filter (pa check-op op) rest)]
;         [mod-rest (lset-difference eq? rest lift-terms)])
;    (cons op (apply append (cons mod-rest
;                           (map (compose cdr vlist-ref') lift-terms))))))

(define (merge-top-level prog-idx)
  (let ([formula (vlist-ref' prog-idx)])
    (if (leaf? formula) prog-idx
        (let* ([op (car formula)]
               [rest (cdr formula)]
               [lift-terms (filter (pa check-op op) rest)]
               [mod-rest (lset-difference eq? rest lift-terms)])
          (lookup (cons op (apply append (cons mod-rest
                                               (map (compose cdr vlist-ref') lift-terms)))))))))


(define (fixed-point f)
  (define (fixed-point-aux f x)
    (let ((next (f x)))
      (if (equal? x next) x (fixed-point-aux f (f x)))))
  (pa fixed-point-aux f))
(define merge-deep (fixed-point merge-top-level))

; assoc and comm rule
; assoc <-- pull up inner list to same level
;  this will help with (+ 1 (+ x0 (+ 1 x0)))
(define (simplify-xor prog-idx) ; currently caches intermediate computations
  (let* ([merged-prog-idx (merge-deep prog-idx)]
         [formula (vlist-ref' merged-prog-idx)])
    (if (leaf? formula) merged-prog-idx
        (let* ([sorted (sort (cdr formula) <)])
          (lookup (match formula
                    [('+ rest ...)
                     (let* ([no-zeroes (delete 0 sorted)]  ; A + 0 -> A
                            [odd-elems (remove-doubles no-zeroes)] ; A + A -> 0
                            [simp-odd-elems (map simplify-xor odd-elems)])
                       (match (length simp-odd-elems)
                         [0 'zero]
                         [1 (car simp-odd-elems)]
                         [else (cons '+ simp-odd-elems)]))]
                    [('* rest ...)
                     (if (eq? 0 (car sorted)) 0
                         (let* ([no-ones (delete 1 sorted)] ; A * 1 -> A
                                [uniq    (delete-duplicates no-ones)] ; A * A -> A
                                [distrib 'todo] ; (A + B)(C + D) -> AC + AD + BC + BD
                                [simp-uniq-elems (map simplify-xor uniq)])
                           (match (length simp-uniq-elems)
                             [0 'one]
                             [1 (car simp-uniq-elems)]
                             [else (cons '* simp-uniq-elems)])))]))))))


; want to make n-ary, by collecting by assoc
; normal commutative form not enough (+ 1 (+ x0 (+ 1 x0)))

(test (unroll (simplify-xor (share '(+ zero x1))))) ;x1
(test (unroll (simplify-xor (share '(* x1 zero))))) ;zero
(test (unroll (simplify-xor (share '(* zero x1))))) ;zero
(test (unroll (simplify-xor (share '(* x1 x1))))) ; x1
(test (unroll (simplify-xor (share '(+ x1 x1))))) ; 0
(test (unroll (simplify-xor (share '(+ (+ x1 x2) (+ x1 x2)))))) ;; zero
(test (unroll (simplify-xor (share '(+ one (+ one x0)))))) ;x0
(test (unroll (simplify-xor (share '(+ one (+ (+ one (+ x0 one))
                                              (* (+ one (+ x0 one))
                                                 (+ one (+ x0 one)))))))))
;; (+ one x0 x0)

(define simplify-xor-complete (fixed-point simplify-xor))
(test (unroll (simplify-xor-complete
               (share '(+ one (+ (+ one (+ x0 one))
                                 (* (+ one (+ x0 one))
                                    (+ one (+ x0 one))))))))) ;one

; issues -- unbelievably inefficient
;  expands out associativity recursively on every rec call
;  has to fixpoint simplify-xor to deal with operator alternation

; order is so messed up
; really just wanted completeness
; but won't get with operator alternations
; I think could get by taking this to fixed point

; works but doesn't reduce everything
; make n-ary xor and logical AND, but I sometimes need to apply n-ary assoc multiple times
; but that gets rid of natural simplifications
; so have to find a balance

;;;;; test speed ;;;;;;;;;


(define (generate-xor-formula prob-expansion decay num-vars)
  ;(generate-xor-formula .9 .8 3) ;(* (+ (* v3 v3) v1) (* v1 v1))
  (define (generate-random-tree prob-of-expansion decay)
    (define (sample-list lst)
      (list-ref lst (random (length lst))))
    (define (generate-random-tree-aux prob-of-expansion)
      (if (> (random:uniform) prob-of-expansion)
          'x
          (let ((bin-op (sample-list '(+ *))))
            (list bin-op
                  (generate-random-tree-aux (* decay prob-of-expansion))
                  (generate-random-tree-aux (* decay prob-of-expansion))))))
    (generate-random-tree-aux prob-of-expansion))
  (define (generate-formula-aux formula-tree)
    (if (leaf? formula-tree)
        (string->symbol
         (string-append "v" (number->string (+ 1 (random num-vars)))))
        (list (car formula-tree)
              (generate-formula-aux (cadr formula-tree))
              (generate-formula-aux (caddr formula-tree)))))
  (let ((formula-tree (generate-random-tree prob-expansion decay)))
    (share (generate-formula-aux formula-tree))))

(define (count-leaves tree)
  (if (leaf? tree) 1 (apply + (map get-tree-size (cdr tree)))))
(define (count-tree-nodes tree)
  (if (leaf? tree) 1 (+ 1 (apply + (map count-tree-nodes (cdr tree))))))
(define (average lst) (exact->inexact (/ (apply + lst) (length lst))))

(define (average-formula-size prob-expansion decay num-vars)
  ; .9 .8 3 -> 10 to 20
  (average
   (map (compose count-tree-nodes unroll)
        (map (lambda (_) (generate-xor-formula prob-expansion decay num-vars)) (iota 100)))))


;(define p (generate-xor-formula .9 .8 3))
;(unroll p)
;;$38 = (+ (+ v2 v3) (* v1 (* v1 v3)))
;(unroll (simplify-xor-complete p))
;;$39 = (+ v2 v3 (* v3 v1))

; what about distrib (* (+ v0 v1) (+ v0 v2))
; not doing anything
; need to add a rule for this

; other version which may be less messy
; have a whole series of rewrite rules
; but this defeats purpose of xor calc
