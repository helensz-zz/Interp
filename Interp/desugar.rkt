#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
;; (define (desugar sexpr))
(define (desugar sexpr)
  (type-case SCFWBAE sexpr
    [idS (i) (id i)]
    [numS (n) (num n)]
    [boolS (b) (bool b)]
    [charS (c) (chaR c)]
    [stringS (s) (strinG s)]
    [listS (l) (lisT (map desugar l))]
    [iF0 (condicion then else) (iF (desugar condicion)
                                   (desugar then)
                                   (desugar else))]
    [iFS (condicion then else) (iF (desugar condicion)
                                   (desugar then)
                                   (desugar else))]
    [opS (f args) (op f (map desugar args))]
    [condS (cases) (desugar-cases cases)]
    [withS (bindings body)
           (app (fun (with-ids bindings) (desugar body))
                (map desugar (with-values bindings)))]
    [withS* (bindings body) (desugar (destar bindings body))]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (map desugar args))]))


;; SECCIÓN DE FUNCIONES AUXILIARES.


#| Elimina el azúcar sintáctica de cada caso en una lista de
 | tipos de dato Condition.
 |#
(define (desugar-cases cases)
  (type-case Condition (car cases)
    [condition (test-expr then-expr)
                 (iF (desugar test-expr)
                     (desugar then-expr)
                     (desugar-cases (cdr cases)))]
    [else-cond (else-expr)
               (desugar else-expr)]))

#| Devuelve una lista con los ids (symbols) de una lista de bindings,
 | la cual a su vez forma parte de un with.
 |#
(define (with-ids bindings)
  (map (λ (b) (binding-id b)) bindings))


#| Devuelve una lista con los values (SCFWBAEs) de una expresión with.
 |#
(define (with-values bindings)
  (map (λ (b) (binding-value b)) bindings))


#| Convierte un with* en with's anidados, para facilitar la eliminación
 | del azúcar sintáctica.
 |#
(define (destar bindings body)
  (cond
    [(empty? bindings) body]
    [else
     (withS (list (car bindings)) (destar (cdr bindings) body))]))


