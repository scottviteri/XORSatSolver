
The boolean satifiability problem can be phrased as checking whether a boolean-valued function on n-variables is in fact the constant 0 function. However, the language that we use when solving SAT problems is incongruous with this interpretation, since a given boolean function may be expressible in multiple ways in terms of logical and and logical or. We see this in the existence of the conjunctive normal form and disjunctive normal forms. But if we were to instead use the algebra of logical and and xor, then we would have a one to one relation between boolean functions and expressions. This leads to a simple and elegant algorithm to check whether a quantifier free formula on boolean variables is satisfiable.

To get an idea of why this may be helpful consider the following workflow. Start with a formula with operations ¬,∧,∨,⇒,⊕(xor), and ⇔, such as (⇒ (∧ x0 x1) (∧ x0 x1)), where x0 and x1 are boolean variables. Transform the formula into one with only ∧ and ⊕. Then simplify the resultant multinomial purely via equational laws of the xor algebra. If the final formula is anything but 0, the constant 0 function, then the original formula is satisfiable.

To write the equational laws, I will write xor as + and logical and as *, and I will sometimes leave multiplication implicit. We have commutativity and associativity -- A+B=B+A, AB=BA, (A+B)+C=A+(B+C), and (AB)C=A(BC). We have top and bottom elements 0 and 1, such that A+0=A, A*0=0, and A*1=A. We have an additive inverse since A+A=0, and we have a kind of degenerate multiplicative inverse since AA=A. Lastly, multiplication distributes over addition -- A(B+C)=AB+AC and (A+B)C=AC+BC. In other words, we are working over a field with two elements.

To transform the formula into xor form, we can derive the following equivalences.
¬A ⇔ 1+A
A∨B ⇔ A+B+AB
A⇒B ⇔ ¬A∨B ⇔ ¬A+B+(¬A)B
    ⇔ (1+A)+B+(1+A)B ⇔ 1+A+B+B+AB ⇔ 1+A+AB
(A⇔B) ⇔ ¬(A+B) ⇔ 1+A+B

To give a full example, again consider the formula (⇒ (∧ x0 x1) (∧ x0 x1)). After translating to xor by recursively applying the rule for implication above, we get (+ 1 (* x0 x1) (* (* x0 x1) (* x0 x1))). I am implicitly using associativity by using n-ary + and * symbols, and I will use full term sharing to prevent duplication of (* x0 x1) in memory. Since AA=A, (* (* x0 x1) (* x0 x1)) reduces to (* x0 x1). Then we have (+ one (* x0 x1) (* (* x0 x1)
Then:

(+ 1 (* x0 x1) (* (* x0 x1) (* x0 x1)))
(+ 1 (* x0 x1) (* x0 x1)) by AA=A
(+ 1 0) by A+A=0
1 by A+0=A

We have not only shown that the original formula is satisfiable, but also that it is the constant 1 function, or valid!

Even with term sharing, it is possible that formulas will become too large during conversion to xor form since A∨B∨C ⇔ A+B+C+AB+BC+AB+ABC. Also I will have to be careful with application of the distributive law during the multinomial simplification phase since (A+B+C)(D+E+F) results in a formula with 9 terms.

Tseytin transformation to CNF
 Davis-Putnam, DLL/DPLL
 Search for choice of variables that satisfy


