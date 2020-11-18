#lang scribble/lp2
@(require scribble/manual "helper.rkt")

@aoc-title[7]

@defmodule[aoc-racket/day07]

@link["http://adventofcode.com/day/7"]{The puzzle}. Our @link-rp["day07-input.txt"]{input} describes an electrical circuit, with each line of the file describing the signal provided to a particular wire.

@chunk[<day07>
       <day07-setup>
       <day07-ops>
       <day07-q1>
       <day07-q2>
       <day07-test>]

@isection{What's the signal on wire @tt{a}?}

The first question we should ask is — how do we model a wire? We're told that it's a thing with inputs that can be evaluated to get a value. So it sounds a lot like a function. Thus, what we'll do is convert our wire descriptions into functions, and then run the function called @racket[a].

In other languages, creating functions from text strings would be a difficult trick. But this facility is built into Racket with @iracket[define-syntax]. Essentially our program will run in two phases: in the syntax-transformation phase, we'll read in the list of wire descriptions and expand them into code that represents functions. In the second phase, the program — including our new functions, created via syntax transformation — will compile & run as usual.

The @racket[convert-input-to-wire-functions] transformer takes the input strings and first converts each into a @italic{datum} — that is, a fragment of Racket code. So an input string like this:

@racket["bn RSHIFT 2 -> bo"]

becomes a datum like this:

@racket[(wire bn RSHIFT 2 -> bo)]

Next, this transformer converts the datums into @italic{syntax}, a process that adds contextual information (for instance, the meanings of identifiers) so the code can be evaluated.

Then the @racket[wire] transformer moves the arguments around to define functions, by matching the three definition patterns that appear in the input. Thus, syntax like this:

@racket[(wire bn RSHIFT 2 -> bo)]

becomes:

@racket[(define (bo) (RSHIFT (evaluate-arg bn) (evaluate-arg 2)))]

@racket[evaluate-arg] lets us handle the fact that some of the arguments for our wires are other wires, and some arguments are numbers. Rather than detect these differences during the syntax-transformation phase, we'll just wrap every input argument with @racket[evaluate-arg], which will do the right thing in the next phase.

(@racket[wire-value-cache] is just a performance enhancement, so that wire values don't have to be computed multiple times.)

One gotcha when using syntax transformers is that identifiers introduced by a transformer can silently override others (in the same way that identifiers defined inside a @iracket[let] will override those with the same name outside the @racket[let]). For instance, one of the wires in our input is named @tt{if}. When our syntax transformer defines the @tt{if} function, it will override the usual meaning of @iracket[if]. There are plenty of elegant ways to prevent these name collisions. (The most important of which is called @italic{syntax hygiene}, and permeates the design of Racket's syntax-transformation system.) But because this is a puzzle, we'll take the cheap way out: we won't use @racket[if] elsewhere in our code, and instead use @iracket[cond].

@chunk[<day07-setup>
       (require racket rackunit
                (for-syntax racket/file racket/string))
       (provide (all-defined-out))

       (define-syntax (convert-input-to-wire-functions stx)
         (syntax-case stx ()
           [(_)
            (let* ([input-strings (file->lines "day07-input.txt")]
                   [wire-strings (map (λ (str) (format "(wire ~a)" str)) input-strings)]
                   [wire-datums (map (compose1 read open-input-string) wire-strings)])
              (datum->syntax stx `(begin ,@wire-datums)))]))

       (define-syntax (wire stx)
         (syntax-case stx (->)
           [(_ arg -> wire-name)
            #'(define (wire-name) (evaluate-arg arg))]
           [(_ 16bit-op arg -> wire-name)
            #'(define (wire-name) (16bit-op (evaluate-arg arg)))]
           [(_ arg1 16bit-op arg2 -> wire-name)
            #'(define (wire-name) (16bit-op (evaluate-arg arg1) (evaluate-arg arg2)))]
           [(_ expr) #'(begin expr)]
           [else #'(void)]))

       (convert-input-to-wire-functions)

       (define wire-value-cache (make-hash))

       (define (evaluate-arg x)
         (cond
           [(procedure? x) (hash-ref! wire-value-cache x (thunk* (x)))]
           [else x]))

       ]

We also need to implement our 16-bit math operations. As we saw above, our syntax transformers are generating code that looks like, for instance, @racket[(RSHIFT (evaluate-arg bn) (evaluate-arg 2))]. This code won't work unless we've defined an @racket[RSHIFT] function too.

These next definitions use @racket[define-syntax-rule] as a shortcut, which is another syntax transformer. (Thanks to @link["https://jeapostrophe.github.io"]{Jay McCarthy} for the 16-bit operations.)


@chunk[<day07-ops>
       (define (16bitize x)
         (define 16bit-max (expt 2 16))
         (define r (modulo x 16bit-max))
         (cond
           [(negative? r) (16bitize (+ 16bit-max r))]
           [else r]))

       (define-syntax-rule (define-16bit id proc)
         (define id (compose1 16bitize proc)))
       (define-16bit AND bitwise-and)
       (define-16bit OR bitwise-ior)
       (define-16bit LSHIFT arithmetic-shift)
       (define-16bit RSHIFT (λ (x y) (arithmetic-shift x (- y))))
       (define-16bit NOT bitwise-not)]


After that, we just evaluate wire function @racket[a] to get our answer.

@chunk[<day07-q1>
       (define (q1) (a))]



@isection{What's the signal on wire @tt{a} if wire @tt{b} is overridden with @tt{a}'s original value?}

Having done the heavy lifting, this is easy. We'll redefine wire function @racket[b] to produce the new value, and then check the value of @racket[a] again.

Ordinarily, as a safety measure, Racket won't let you redefine functions. But we can circumvent this limitation by setting @iracket[compile-enforce-module-constants] to @racket[#f]. We'll also need to reset our cache, since this change will affect the other wires too.



@chunk[<day07-q2>
       (compile-enforce-module-constants #f)

       (define (q2)
         (define first-a-val (a))
         (set! b (thunk* first-a-val))
         (set! wire-value-cache (make-hash))
         (a))
       ]


@section{Testing Day 7}

@chunk[<day07-test>
       (module+ test
         (check-equal? (q1) 46065)
         (check-equal? (q2) 14134))]
