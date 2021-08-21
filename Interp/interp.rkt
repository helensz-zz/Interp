#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE
;; (define (lookup name ds)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup (string-append "Variable libre: " (~v name)))]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))


;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
(define (interp expr ds)
  (type-case CFWBAE expr
    [id (i)
        (lookup i ds)]
    [num (n)
         (numV n)]
    [bool (b)
          (boolV b)]
    [chaR (c)
          (charV c)]
    [strinG (s)
            (stringV s)]
    [lisT (l)
          (listV (map (λ (e) (interp e ds)) l))] 
    [iF (condicion then else)
        (cond
          [(boolV? (interp condicion ds))
           (if (equal? (interp condicion ds) (boolV #t))
            (interp then ds)
            (interp else ds))]
          [else
           (error 'interp "Símbolo no esperado. La condicional de if, no es un booleano")])]
    [op (f args)
        (opV f (map (λ (e) (interp e ds)) args))]
    [fun (params body)
         (closure params body ds)]
    [app (fun args)
         (let* ([fun-val (interp fun ds)]
               [param (closure-param fun-val)]
               [env (closure-env fun-val)])
           (interp (closure-body fun-val)
                   (ext-ds param args env ds)))]))


;;  FUNCIONES AUXILIARES

#|
 | Función que se encarga de extender el ambiente
 | para la evaluación de una función
 | opV: (listof symbol) (listof CFWBAE) (DefrdSub) (DefrdSub) -> DefrdSub
 |#
(define (ext-ds param args env ds)
  (cond
    [(empty? param) env]
    [else
     (ext-ds (cdr param)
             (cdr args)
             (aSub (car param)
                   (interp (car args) ds)
                   env)
             ds)]))

#|
 | Función que realiza la operación "f" recibida y
 | devuelve el Value correspondiente.
 | opV: procedure CFWBAE-Value -> CFWBAE-Value
 |#
(define (opV f argsV)
  (cond
    ;; Procedures que devuelven num-values
    [(equal? f +)
     (numV (apply + (map numV-n argsV)))]
    [(equal? f -)
     (numV (apply - (map numV-n argsV)))]
    [(equal? f /)
     (numV (apply / (map numV-n argsV)))]
    [(equal? f *)
     (numV (apply * (map numV-n argsV)))]
    [(equal? f add1)
     (numV (apply add1 (map numV-n argsV)))]
    [(equal? f sub1)
     (numV (apply sub1 (map numV-n argsV)))]
    [(equal? f modulo)
     (numV (apply modulo (map numV-n argsV)))]
    [(equal? f expt)
     (numV (apply expt (map numV-n argsV)))]
    [(equal? f length)
     (numV (apply length (map listV-l argsV)))]
    [(equal? f string-length)
     (numV (apply string-length (map stringV-s argsV)))]
    
    ;; Procedures que devuelven bool-values
    [(equal? f anD)
     (boolV (apply anD (map boolV-b argsV)))]
    [(equal? f oR)
     (boolV (apply oR (map boolV-b argsV)))]
    [(equal? f <)
     (boolV (apply < (map numV-n argsV)))]
    [(equal? f <=)
     (boolV (apply <= (map numV-n argsV)))]
    [(equal? f =)
     (boolV (apply = (map numV-n argsV)))]
    [(equal? f >)
     (boolV (apply > (map numV-n argsV)))]
    [(equal? f >=)
     (boolV (apply >= (map numV-n argsV)))]
    [(equal? f not)
     (boolV (apply not (map boolV-b argsV)))]
    [(equal? f zero?)
     (boolV (apply zero? (map numV-n argsV)))]
    [(equal? f number?)
     (boolV (apply number? (map numV-n argsV)))]
    [(equal? f boolean?)
     (boolV (apply boolean? (map boolV-b argsV)))]
    [(equal? f char?)
     (boolV (apply char? (map charV-c argsV)))]
    [(equal? f string?)
     (boolV (apply string? (map stringV-s argsV)))]
    [(equal? f list?)
     (boolV (apply list? (map listV-l argsV)))]
    [(equal? f empty?)
     (boolV (apply empty? (map listV-l argsV)))]

    ;; Procedures que devuelven list-values
    [(equal? f car)
     (listV (list (apply car (map listV-l argsV))))]
    [(equal? f cdr)
     (listV (apply cdr (map listV-l argsV)))]
    [(equal? f cons)
     (listV (apply cons (car argsV) (map listV-l (cdr argsV))))]
    [(equal? f append)
     (listV (apply append (map listV-l argsV)))]
    
    ;; Procedures que devuelven string-values
    [(equal? f string-append)
     (stringV (apply string-append (map stringV-s argsV)))]))