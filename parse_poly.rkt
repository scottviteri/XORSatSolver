#lang racket

(require "polynomial-reduction.rkt")
(require brag)
(require brag/support)
(require "poly.rkt")

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

(provide parse tokenize interpret)
