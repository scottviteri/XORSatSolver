#lang scribble/lp2
@(require scribble/eval)
@(require scribble-math)
@(use-katex)

@title[#:style (with-html5 manual-doc-style)]{tsra}
tsrZooasp bop beepr sr
@section{The con}
That ``squeak'' was the mouse
@section{tsr}
@bold{tsra}

@centered{@bold{@italic{hi there}}}
@itemlist[@item{first} @item{second}]
@chunk[<f>
       (define (f x) <args>)]
@chunk[<args>
       (+ x x)]
@examples[(+ 1 1)]
@racketblock[(define (f x) (* x x))]

Some inline math @($ "x^2").

tr @($$ "\\sum_{i=0}^n x_i^3") tsra
