#lang plai 

;funcion que nos dice si algo es un atomo de racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;1.- MArray
(define-type Array
  [MArray (n number?) (l list?)])

;(MArreglo 4 (list 1 2 3))

;2.-LIST
(define-type MList
  [MEmpty]
  [MCons(n atom?)(rest MList?)])

(test (MEmpty) (MEmpty))
(test (MCons 1 (MCons 2 (MCons 3 (MEmpty)))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))

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
(define-type Figure
  [Circle (n 2D-Point?) (m number?)]
  [Square (n 2D-Point?) (m number?)]
  [Rectangle (n 2D-Point?) (m number?) (o number?)])

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

;2.-MArray2MList
(define (MArray2MList array)
  (cond
    [(or (empty? (MArray-l array)) (= 0 (MArray-n array))) (MEmpty)]
    [(MCons (car (MArray-l array)) (MArray2MList (MArray (- (MArray-n array) 1) (cdr (MArray-l array)))))]))

(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 5 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))
(test (MArray2MList (MArray 3 '(1 2 3))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))

;3-PrintML
(define (printML ml)
  (if (MEmpty? ml)
      "[]"
      (if(MEmpty? (MCons-rest ml))
        (string-append "[" (~a (MCons-n ml)) "]")
         (string-append "[" (auxPrint ml) "]"))))
 
(define (auxPrint ml)
  (if (MEmpty? ml)
      ""
      (if(MEmpty? (MCons-rest ml))
         (string-append (~a (MCons-n ml)))
         (string-append (~a (MCons-n ml)) ", " (auxPrint (MCons-rest ml))))))

;(printML (LVacia))
;(printML (MCons 7 (LVacia)))
;(printML (MCons 7 (MCons 4 (MCons 10 (LVacia)))))
;(printML (MCons 7 (MCons 4 (MCons 10 (MCons 5 (LVacia))))))


;4-ConcatMl
(define (concatML ml1 ml2)
  (if(MEmpty? ml1)
     ml2
      (MCons (MCons-n ml1) (concatML (MCons-rest ml1) ml2))))

;(concatML (MCons 7 (MCons 4 (LVacia))) (MCons 1 (LVacia)))
;(concatML (MCons 7 (MCons 4 (LVacia))) (MCons 1 (MCons 10 (LVacia))))
;(printML (concatML (MCons 7 (MCons 4 (LVacia))) (MCons 1 (MCons 10 (LVacia)))))


;5-lengthML
(define (lengthML ml)
  (if(MEmpty? ml)
     0
     (+ 1 (lengthML (MCons-rest ml)))))
;(lengthML (LVacia))
;(lengthML (MCons 7 (MCons 4 (MCons 5 (MCons 1 (LVacia))))))

;6.-mapML
(define (mapML fun list)
  (cond
    [(MEmpty? list) (MEmpty)]
    [(MCons (fun (MCons-n list)) (mapML fun (MCons-rest list)))]))

(test (mapML add1 (MCons 7 (MCons 4 (MEmpty)))) (MCons 8 (MCons 5 (MEmpty))))
(test (mapML (lambda (x) (* x x)) (MCons 10 (MCons 3 (MEmpty)))) (MCons 100 (MCons 9 (MEmpty))))

;7.-Filter
(define (filterML predicate list)
  (cond
    [(MEmpty? list) (MEmpty)]
    [(eq? (predicate (MCons-n list)) #t) (MCons (MCons-n list) (filterML predicate (MCons-rest list)))]
    [else (filterML predicate (MCons-rest list))]))

(test (filterML (lambda (x) (not (zero? x))) (MCons 2 (MCons 0 (MCons 1 (MEmpty))))) (MCons 2 (MCons 1 (MEmpty))))

;---se definen los siguientes tipos de datos y valores-----------

(define-type Coordinates
[GPS (lat number?)
(long number?)])

(define-type Location
[building (name string?)
(loc GPS?)])


;; Coordenadas GPS
(define gps-satelite (GPS 19.510482 -99.23411900000002))
(define gps-ciencias (GPS 19.3239411016 -99.179806709))
(define gps-zocalo (GPS 19.432721893261117 -99.13332939147949))
(define gps-perisur (GPS 19.304135 -99.19001000000003))

(define plaza-satelite (building "Plaza Satelite" gps-satelite))
(define ciencias (building "Facultad de Ciencias" gps-ciencias))
(define zocalo (building "Zocalo" gps-zocalo))
(define plaza-perisur (building "Plaza Perisur" gps-perisur))
(define plazas (MCons plaza-satelite (MCons plaza-perisur (MEmpty))))


;8.-haversine

;9.-gps-coordinates

;10.-closest-building

;11.-building-at-distance

;12.-area
(define (area figure)
    (type-case Figure figure
      (Circle (point m) (* pi (expt m 2)))
      (Rectangle (point m o)(* m o))
      (Square(point m)(* m m))))

;13.-in-figure?

#|(define (in-figure? figure point)
  
 (type-case Position point
  (2D-Point(x y) 
   (type-case Figure figure
     (Square (point m)(if  )    )
    
     ))))

|#




