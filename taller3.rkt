#lang eopl

;Especificación Léxica

(define scanner-spec-simple-interpreter
'(
  (white-sp (whitespace) skip)
  (comentario ("//" (arbno (not #\newline))) skip)
  (identificador ("@" (arbno (or letter digit "?"))) symbol)
  (texto ("\""(arbno letter)"\"") string)
  (numero (digit (arbno digit)) number)
  (numero ("-" digit (arbno digit)) number)
  (numero (digit (arbno digit)(or "," ".") digit (arbno digit)) number)
  (numero ("-" digit (arbno digit)(or "," ".") digit (arbno digit)) number)
 )
)

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '(
    (programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion (identificador) var-exp)
    (expresion (texto) texto-lit)
    (expresion ("("expresion primitiva-binaria expresion")") primapp-bin-exp)
    (expresion (primitiva-unaria "("expresion")") primapp-un-exp)
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta) 
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
   )
  )
;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))