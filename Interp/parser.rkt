#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::= <number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(char? sexp) (charS sexp)]
    [(string? sexp) (stringS sexp)]
    [(empty? sexp)
     (error 'parser "Las expresiones vacías no son válidas")]
    [(list? sexp)
     (case (car sexp)
       [(true) (boolS #t)]
       [(false) (boolS #f)]
       [(+ - * / < <= = > >= anD oR)
        (opS (select-multiary (car sexp)) (map parse (cdr sexp)))]
       [(modulo expt cons append string-append)
        (if (not (equal? (length sexp) 3))
            (error 'parser "Error de aridad")
            (opS (select-binary (car sexp))
                 (list (parse (second sexp))
                       (parse (third sexp)))))]
       [(add1 sub1 not zero? number? boolean? char? string?
              list? empty? length car cdr string-length)
        (if (not (equal? (length sexp) 2))
            (error 'parser "Error de aridad")
            (opS (select-unary (car sexp))
                 (list (parse (second sexp)))))]
       [(lst)
        (listS (map parse (cdr sexp)))]
       [(if if0)
        (if (not (equal? (length sexp) 4))
            (error 'parser "Falta la else-expresion")
            ((select-if (first sexp)) (parse (second sexp)) (parse (third sexp)) (parse (fourth sexp))))]
       [(cond)
        (case (first (last sexp)) 
          [(else) (condS (map (λ p (parse-cond-pair (car p))) (cdr sexp)))]
          [else (error 'parser "Falta la else-expresion")])]
       [(with)
        (withS (parse-bindings (second sexp)) (parse (third sexp)))]
       [(with*)
        (withS* (parse-bindings (second sexp)) (parse (third sexp)))]
       [(fun)
        (let ([param (check-duplicates (second sexp))])
          (if (equal? param #f)
            (funS (second sexp) (parse (third sexp)))
            (error 'parser (string-append "Parámetro repetido" (~v param)))))]
       [else (appS (parse (first sexp))
              (map parse (second sexp)))])]))


;;  FUNCIONES AUXILIARES.


#| Devuelve la operacion aritmetica seleccionada.
 | Las opciones posibles son las operaciones aritméticas +, *, -, /; y
 | los operadores de comparación <, <=, >, >= y =.
 |#
(define (select-multiary op)
  (case op
    [(+) +]
    [(-) -]
    [(/) /]
    [(*) *]
    [(<) <]
    [(<=) <=]
    [(=) =]
    [(>) >]
    [(>=) >=]
    [(oR) oR]
    [(anD) anD]))


#| Devuelve la operacion unaria seleccionada.
 | Las opciones posibles son add1, sub1, not y el predicado zero?.
 |#
(define (select-unary op)
  (case op
    [(add1) add1]
    [(sub1) sub1]
    [(not) not]
    [(zero?) zero?]
    [(number?) number?]
    [(boolean?) boolean?]
    [(char?) char?]
    [(string?) string?]
    [(list?) list?]
    [(empty?) empty?]
    [(length) length]
    [(car) car]
    [(cdr) cdr]
    [(string-length) string-length]))


#| Devuelve la operacion binaria seleccionada entre modulo y expt.
 | Las opciones posibles son modulo y expt.
 |#
(define (select-binary op)
  (case op
    [(modulo) modulo]
    [(expt) expt]
    [(cons) cons]
    [(append) append]
    [(string-append) string-append]))

#| Devuelve el if correspondiente.
 |#
(define (select-if ifs)
  (case ifs
    [(if) iFS]
    [(if0) iF0]))


#| Dado un par con componentes de un condicional, lo transforma a
 | un tipo de dato Conditional.
 |#
(define (parse-cond-pair cond-pair)
  (case (car cond-pair)
    [(else) (else-cond (parse (second cond-pair)))]
    [else (condition (parse (car cond-pair))
                     (parse (second cond-pair)))]))


#| Parsea una lista de pares id-valor, permitiendo o no duplicados        
 |#
(define (parse-bindings list-of-bindings)
      (map (lambda (bnd) (binding (first bnd) (parse (cadr bnd))))
           list-of-bindings))


#|
 | Realiza la operación booleana OR con un par de elementos.
 |#
(define (or-pair b1 b2)
  (if b1 #t b2))


#| Redefinición de la operación booleana OR.
 |#
(define (oR xs)
  (foldr or-pair #f xs))


#| Realiza la operación booleana AND con un par de elementos.
 |#
(define (and-pair b1 b2)
  (if b1 b2 #f))


#| Redefinición de la operación booleana AND.
 |#
(define (anD xs)
  (foldr and-pair #t xs))

