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
    (expresion ("Si" expresion "entonces" expresion "sino" expresion "finSI") if-exp)
    (expresion ("declare" "("(arbno identificador "=" expresion ";" )   ")" "{" expresion"}") variableLocal-exp)
    (expresion ("procedimiento" "("(arbno identificador)")" expresion) procedimiento-exp)

    
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
  
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extended-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero | string
; evalua la expresión en el ambiente de entrada
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (datum) datum)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable env id))
      (primapp-un-exp (prim rand)
                   (apply-primitive-un prim (evaluar-expresion rand env)))
      (primapp-bin-exp (rand1 prim rand2)
                   (apply-primitive-bin prim (evaluar-expresion rand1 env) (evaluar-expresion rand2 env))
                   )
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (evaluar-expresion test-exp env))
                  (evaluar-expresion true-exp env)
                  (evaluar-expresion false-exp env)))                   
      (variableLocal-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (evaluar-expresion body
                                  (extended-env ids args env))))
      (procedimiento-exp(lista-ID exp)
               (cerradura lista-ID exp env))
      )))


;apply-primitive-bin: <primitiva> <expresion> <expresion> -> numero | string
(define apply-primitive-bin
  (lambda (prim args1 args2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ args1 args2))
      (primitiva-resta () (- args1 args2))
      (primitiva-multi () (* args1 args2))
      (primitiva-div () (/ args1 args2))
      (primitiva-concat () (string-append args1 args2))
      )))

;apply-primitive-un: <primitiva> <expresion> <expresion> -> numero
(define apply-primitive-un
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1))
      )))

;***********************Funciones Auxiliares******************************

;Funcion que retorna la posicion en la que el caracter esta ubicado
(define indice-lista
  (lambda (pred lst)
    (indice-lista-aux 0 pred lst)))

;; Función auxiliar que realiza el llamado recursivo utilizando como criterio principal
;; un contador auxiliar que ayuda a determinar el recorrido dentro de la lista
(define indice-lista-aux
  (lambda (n pred lst)
    (if (null? lst)
        'NoCumple
        (if (pred (car lst))
            n
            (indice-lista-aux (+ n 1) pred (cdr lst))))))

;buscar-pos
;Funcion auxiliar que retorna el elemento que está ubicado en la posicion dada
(define (buscar-pos lst n)
  (if (zero? n)
      (car lst)
      (buscar-pos (cdr lst) (- n 1))))


;******************************************** Ambientes *******************************************
;****Gramatica*******
;<env-exp> ::= (empty-env)
;          ::= (extend-env <list-of symbol>
;                          <list-of scheme-value> <env-exp>)

;Representación
(define-datatype environment environment?
   (empty-env)
   (extended-env (syms (list-of symbol?))
                        (vals (list-of scheme-value?))
                        (env environment?)))

(define scheme-value? (lambda (v) #t))

;función que busca un símbolo en un ambiente dado
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      ;No se encontró la variable buscada
      (empty-env ()
                   (eopl:error '"Error, la variable no existe"))
      ;busque en la lista de variables si esta la que se requiere
      (extended-env (syms vals env)
                           (let ((pos (indice-lista (lambda (x) (eqv? x sym)) syms)))
                             (if (number? pos)
                                 ; en caso si, traiga la posicion, y busque esa posicion en la lista de valores que le corresponde
                                 (buscar-pos vals pos)
                                 ; en caso no, busque de nuevo quitando esa parte ya buscada
                                 (buscar-variable env sym)))))))
                                 
;; función para probar booleanos
(define true-value?
  (lambda(x)
    (not (zero? x))))
    
;;;;funciones auxiliares para la expresion declare

;;eval-rands evalua los operandos y los convierte en un ambiente
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))
;;eval-rand ingresa el operando y lo llama para evaluar la expresion
(define eval-rand
  (lambda (rand env)
    (evaluar-expresion rand env)))

;******************************************** Procedimientos *******************************************
(define-datatype procVal procVal?
  (cerradura
   (lista-ID(list-of symbol?))
   (exp expresion?)
   (amb environment?)))



(interpretador)
    
