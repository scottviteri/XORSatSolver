#lang brag


plus_expr : mult_expr (/ PLUS mult_expr)*
mult_expr : factor (/ TIMES factor)*
@factor : variable | NUM  | LEFT-PAREN plus_expr RIGHT-PAREN
variable : VAR /EXP NUM | VAR
