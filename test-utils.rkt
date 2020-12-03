#lang racket

(require (only-in srfi/1 fold))

(define id (lambda (x) x))
(define (curry f x) (lambda (y) (f x y)))
(define (flip f) (lambda (x y) (f y x)))
(define (run f) (f))
(define const (lambda (x) (lambda (y) x)))
(define (compose-2 f g h) (lambda (x) (f (g x) (h x))))

(define leaf? (compose not list?))

(define (get-vars t)
  ;(get-vars '(∧ (⇒ (⇒ (¬ v4) v3) v3) (∨ (¬ v4) (∧ (¬ v5) (¬ v5))))) ;'((¬ v4) v3 v3 (¬ v4) (¬ v5) (¬ v5))
  (match t
    [x #:when (leaf? x) (list x)]
    [(list op x) (get-vars x)]
    [(list op x y) (append (get-vars x) (get-vars y))]
    [(cons op l) (fold append '() (map get-vars l))]))

(define (count-operations t)
  ;(count-operations '(∧ (⇒ (⇒ (¬ v4) v3) v3) (∨ (¬ v4) (∧ (¬ v5) (¬ v5))))) ;5
  ;(count-operations '(∧ (∨ (¬ v2) (¬ v1) (¬ v1) v3 v1 (¬ v1)) (∨ (¬ v2) (¬ v1) (¬ v1) (¬ v1) (¬ v1)) (∨ (¬ v2) (¬ v1) (¬ v1) v1 (¬ v1)) (∨ (¬ v2) 2 (¬ v1) v3 v1 (¬ v1)) (∨ (¬ v2) 2 (¬ v1) (¬ v1) (¬ v1)) (∨ (¬ v2) 2 (¬ v1) v1 (¬ v1)))) ;7
  (match t
    [x #:when (leaf? x) 0]
    [(list op x) (+ 1 (count-operations x))]
    [(list op x y) (+ 1 (count-operations x) (count-operations y))]
    [(cons op l) (+ 1 (apply + (map count-operations l)))]))

(define (make-vars-unique lst)
  (fold (lambda (next acc) (if (member (abs next) acc)
                          acc
                          (cons (abs next) acc)))
        '() lst))

(define count-vars
  ;(count-vars '(∧ (∨ 2 4) v3)) ;3
  (compose length get-vars))

(define count-unique-vars
  (compose length remove-duplicates get-vars))

(define (tree-size t)
  ;(tree-size '(∧ (∨ 2 4) v3)) ;5
  (if (leaf? t) 1
      (if (eq? 2 (length t))
          (+ 1 (tree-size (cadr t)))
          (+ 1 (tree-size (cadr t)) (tree-size (caddr t)))) ))
(define average (compose-2 / (curry apply +) length))

(define (display-list lst) (map (lambda (x) (begin (display x) (newline))) lst))
(define (enumerate-and-display-list lst)
  (display-list (map cons (range (length lst)) lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Guess whether a formula is valid by random sampling of models ;;;;;;;;;;;
;;;;;;;;;  Inaccurate but computationally efficient                     ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-formula prob-expansion decay num-vars)
  ;(generate-formula .9 .8 3) ;(∨ (¬ v2) (∨ v3 (¬ v3))) <-- random
  (define (generate-random-tree prob-of-expansion decay)
    (define (sample-list lst)
      (list-ref lst (random (length lst))))
    (define (generate-random-tree-aux prob-of-expansion)
      (if (> (random) prob-of-expansion)
          'x
          (let ((bin-op (sample-list '(∧ ∨ ⇒))))
            (list bin-op
                  (generate-random-tree-aux (* decay prob-of-expansion))
                  (generate-random-tree-aux (* decay prob-of-expansion))))))
    (generate-random-tree-aux prob-of-expansion))
  (define (generate-formula-aux formula-tree)
    (if (leaf? formula-tree)
        (if (equal? 0 (random 2))
            (string->symbol (string-append "v" (number->string (+ 1 (random num-vars)))))
            (list '¬
                  (string->symbol (string-append "v" (number->string (+ 1 (random num-vars)))))))
        (list (car formula-tree)
              (generate-formula-aux (cadr formula-tree))
              (generate-formula-aux (caddr formula-tree)))))
  (let ((formula-tree (generate-random-tree prob-expansion decay)))
    (generate-formula-aux formula-tree)))

(define (time-milli f x)
  (let ([t (current-milliseconds)]
        [y (f x)])
    (- (current-milliseconds) t)))

(provide count-vars count-unique-vars get-vars count-operations tree-size generate-formula time-milli)
