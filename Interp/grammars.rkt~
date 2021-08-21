#lang plai
;; Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (value SCFWBAE?)])

;;Definición del tipo condition para la definición de cond.
(define-type Condition
  [condition (test-expr SCFWBAE?) (then-expr SCFWBAE?)]
  [else-cond (else-expr SCFWBAE?)])

;; Definición del tipo SCFWBAEL
(define-type SCFWBAE
  [idS     (i symbol?)]
  [numS    (n number?)]
  [boolS   (b boolean?)]
  [charS   (c char?)]
  [stringS (s string?)]
  [listS   (l (listof SCFWBAE?))]
  [iFS     (test-expr SCFWBAE?) (then SCFWBAE?) (else SCFWBAE?)]
  [iF0     (test-expr SCFWBAE?) (then SCFWBAE?) (else SCFWBAE?)]
  [opS     (f procedure?) (args (listof SCFWBAE?))]
  [condS   (cases (listof Condition?))]
  [withS   (bindings (listof binding?)) (body SCFWBAE?)]
  [withS*  (bindings (listof binding?)) (body SCFWBAE?)]
  [funS    (params (listof symbol?)) (body SCFWBAE?)]
  [appS    (fun SCFWBAE?) (args (listof SCFWBAE?))])


;; Gramáticas de la práctica anterior. Esto lo utilizarás sólo si
;; quieres ganar los puntos extra. Puedes eliminarlo, en otro caso.
;; Definición del tipo CFWBAE
(define-type CFWBAE
  [id     (i symbol?)]
  [num    (n number?)]
  [bool   (b boolean?)]
  [chaR   (c char?)]
  [strinG (s string?)]
  [lisT   (l (listof CFWBAE?))]
  [iF     (condicion CFWBAE?) (then CFWBAE?) (else CFWBAE?)]
  [op     (f procedure?) (args (listof CFWBAE?))]
  [fun    (params (listof symbol?)) (body CFWBAE?)]
  [app    (fun CFWBAE?) (args (listof CFWBAE?))])

;; Data-type que representa un caché de sustituciones
(define-type DefrdSub
  [mtSub]
  [aSub  (name symbol?) (value CFWBAE-Value?) (ds DefrdSub?)])

;;Data-type que representa la sintaxis abstracta de CFWBAE-Value
(define-type CFWBAE-Value
  [closure  (param (listof symbol?)) (body CFWBAE?) (env DefrdSub?)]
  [numV     (n number?)]
  [boolV    (b boolean?)]
  [charV    (c char?)]
  [stringV  (s string?)]
  [listV    (l (listof CFWBAE-Value?))])
