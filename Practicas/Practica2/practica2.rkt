#lang plai 



;1.- MArray
(define-type Array
  [MArray (n number?) (l list?)])

;(MArreglo 4 (list 1 2 3))

;2.-LIST
(define-type MLista
  [LVacia]
  [MCons (n number?)
         (rest MLista?)])

;(MCons 7 (MCons 4 (MCons 10 (LVacia))))

;3.-NTree
(define-type ArbolN
  [HVaciaN]
  [NodoN (n number?)
         (l (listof ArbolN?))])

;(HVaciaN)
;(NodoN 1 (list (HVaciaN)(HVaciaN)(HVaciaN)))

;4.-Position
(define-type Position
  (2D-Point (n number?) (m number?)))

;5.-Figure


;------------------------------------------------------------------------------------------------------------------------------------------
;SECCION 2

;1-SetValueA
(define (coloca l pos val count)
  (cond
    [(< count pos) (cons (car l) (coloca (cdr l) pos val (add1 count)))]
    [(> count pos) empty]
    [(= count pos) (cons val (cdr l))]))

(define (setvalueA ar pos val)
  (if (not(Array? ar))
      (error 'setvalueA "Unknown Type")
      (if (> pos (- (MArray-n ar) 1))
          (error 'setvalueA "Out of bounds")
          (coloca (MArray-l ar) pos val 0))))

;(define ar (MArreglo 5 '(0 0 0 0 0)))
;(setvalueA ar 2 4)
;(setvalueA ar 4 4)
;(setvalueA ar 5 4)

;3-PrintML
(define (printML ml)
  (if (LVacia? ml)
      "[]"
      (if(LVacia? (MCons-rest ml))
        (string-append "[" (~a (MCons-n ml)) "]")
         (string-append "[" (auxPrint ml) "]"))))
 
(define (auxPrint ml)
  (if (LVacia? ml)
      ""
      (if(LVacia? (MCons-rest ml))
         (string-append (~a (MCons-n ml)))
         (string-append (~a (MCons-n ml)) ", " (auxPrint (MCons-rest ml))))))

;(printML (LVacia))
;(printML (MCons 7 (LVacia)))
;(printML (MCons 7 (MCons 4 (MCons 10 (LVacia)))))
;(printML (MCons 7 (MCons 4 (MCons 10 (MCons 5 (LVacia))))))


;4-ConcatMl
(define (concatML ml1 ml2)
  (if(LVacia? ml1)
     ml2
      (MCons (MCons-n ml1) (concatML (MCons-rest ml1) ml2))))

;(concatML (MCons 7 (MCons 4 (LVacia))) (MCons 1 (LVacia)))
;(concatML (MCons 7 (MCons 4 (LVacia))) (MCons 1 (MCons 10 (LVacia))))
;(printML (concatML (MCons 7 (MCons 4 (LVacia))) (MCons 1 (MCons 10 (LVacia)))))


;5-lengthML
(define (lengthML ml)
  (if(LVacia? ml)
     0
     (+ 1 (lengthML (MCons-rest ml)))))
;(lengthML (LVacia))
;(lengthML (MCons 7 (MCons 4 (MCons 5 (MCons 1 (LVacia))))))







