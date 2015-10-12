#lang plai

(require "practica4-base.rkt")

(print-only-errors true)

(define (desugar expr)
  ;; Implementar desugar
  (type-case FAES expr
    [numS (n) (num n)]
    [idS (v) (id v)]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (bindings body) (app (fun (get-Name bindings) (desugar body)) (get-Value bindings))]
    [with*S (bindings body) (aux bindings body)]; with arreglado se utiliza funcion auxiliar 
    [funS (params body) (fun params (desugar body))]
    [appS (funS args) (app (desugar funS) (get-FAE args))]))
    
 ;funcion auxiliar para with*
(define (aux bindings body)
   (if (empty? bindings)
                (desugar body)
                (desugar (withS (list (car bindings)) (with*S (cdr bindings) body)))))
    
  (define (get-Name ds)
  (cond
    [(empty? ds) empty]
    [else  (type-case Binding (car ds)
        [bind (name val) (cons name (get-Name (cdr ds)))])]))

  (define (get-Value ds)
    (cond
      [(empty? ds) empty]
      [else (type-case Binding (car ds)
          [bind (name val) (cons (desugar val) (get-Value (cdr ds)))])]))

  (define (get-FAE ds)
  (if (empty? ds) empty
      (cons (desugar (car ds)) (get-FAE (cdr ds))))) 



(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))

(define (cparse sexp)
  (desugar (parse sexp)))

;interp
(define (interp expr ds)
  (type-case FAE expr
   [num (n) (numV n)]
   [binop (f l r) (numV (f (numV-n (interp l ds)) (numV-n (interp r ds))))]
   [id (v) (lookup v ds)]
   [fun (params body) (closureV params body ds)]
   [app (fun args)
         (local([define funV (interp fun ds)])
           (if (closureV? funV)
               (let* ([funV-params (closureV-param funV)]
                      [nparams (length funV-params)]
                      [nargs (length args)])
                 (interp (closureV-body funV)
                         (if (= nparams nargs) (mParam funV-params args ds (closureV-env funV))
                         (error 'interp (~a "Arity mismatch")))))
           (error 'interp (string-append (~a funV) "Invalid Expresion"))))]))

;look up para interp (shiram)
(define (lookup name ds)
  (type-case Env ds
    [mtSub () (error  'lookup "no binding for identifier" )]
    [aSub (bound-name bound-value rest-ds)
            (if (symbol=? bound-name name)
                 bound-value
                  (lookup name rest-ds))]))
;;Aux Fun for multi param app
(define (mParam params args args-ds a-ds)
  (if (empty? params) a-ds
      (mParam (cdr params) (cdr args) args-ds
                   (aSub (car params)
                         (interp (car args) args-ds) a-ds))))

(define (rinterp expr)
  (interp expr (mtSub)))

(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))
