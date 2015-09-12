#lang plai 
(require racket/math)

;funcion que nos dice si algo es un atomo de racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;1.- MArray
(define-type Array
  [MArray (n number?) (l list?)])
  
  (test(MArray 4 '(1 2 3)) (MArray 4 '(1 2 3)))
  (test(MArray 4 '(1 2 3)) (MArray 4 '(1 2 3)))



;2.-LIST
(define-type MLista
  [MEmpty]
  [MCons(n atom?)(rest MLista?)])
  
(test (MEmpty) (MEmpty))
(test (MCons 1 (MCons 2 (MCons 3 (MEmpty)))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
(test (MCons 1 (MCons 2 (MCons 4 (MCons 5 (MEmpty))))) (MCons 1 (MCons 2 (MCons 4 (MCons 5 (MEmpty))))))
(test (MCons 1 (MCons 2 (MEmpty))) (MCons 1 (MCons 2 (MEmpty))))



;3.-NTree
(define-type ArbolN
  [HVaciaN]
  [NodoN (n number?)
         (l (listof ArbolN?))])
         
         
         (test(HVaciaN) (HVaciaN))
(test(NodoN 1 (list (HVaciaN) (HVaciaN) (HVaciaN))) (NodoN 1 (list (HVaciaN) (HVaciaN) (HVaciaN))) )
(test  (NodoN 1 (list (NodoN 2 (list (HVaciaN)))
                      (NodoN 3 (list (HVaciaN)))
                      (NodoN 4 (list (HVaciaN) (HVaciaN) (HVaciaN)))))
       (NodoN 1(list
(NodoN 2 (list (HVaciaN)))
(NodoN 3 (list (HVaciaN)))
(NodoN 4 (list (HVaciaN) (HVaciaN) (HVaciaN))))) )
(test(NodoN 1 (list (HVaciaN)(HVaciaN)(HVaciaN))) (NodoN 1 (list(HVaciaN) (HVaciaN) (HVaciaN))))
(test(NodoN 1 (list (NodoN 2(list (HVaciaN) (HVaciaN) (HVaciaN))))) (NodoN 1(list(NodoN 2 (list (HVaciaN) (HVaciaN) (HVaciaN))))))
(test(NodoN 1 (list (NodoN 2(list (NodoN 3 (list (HVaciaN) (HVaciaN) (HVaciaN))))))) (NodoN 1(list(NodoN 2 (list (NodoN 3 (list (HVaciaN) (HVaciaN) (HVaciaN))))))))
(test(NodoN 1(list(NodoN 2(list(HVaciaN)))
                  (NodoN 3(list(HVaciaN)))
                  (NodoN 4(list(HVaciaN)))
                  (NodoN 5(list(HVaciaN)(HVaciaN)(HVaciaN))))) (NodoN 1(list(NodoN 2(list(HVaciaN)))
                  (NodoN 3(list(HVaciaN)))
                  (NodoN 4(list(HVaciaN)))
                  (NodoN 5(list(HVaciaN)(HVaciaN)(HVaciaN))))))




;4.-Position
(define-type Position
  (2D-Point (n number?) (m number?)))
  
  (test(2D-Point 1 2) (2D-Point 1 2))
(test(2D-Point 10 (sqrt 20)) (2D-Point 10  (sqrt 20)))
(test(2D-Point 1 (+ 4 5)) (2D-Point 1 9))
(test(2D-Point 21 (-  20 10)) (2D-Point 21 10))

;5.-Figure
(define-type Figure
  [Circle (n 2D-Point?) (m number?)]
  [Square (n 2D-Point?) (m number?)]
  [Rectangle (n 2D-Point?) (m number?) (o number?)])
  
  (test(Circle (2D-Point 4 (sqrt 7)) 5) (Circle(2D-Point 4 (sqrt 7)) 5))
(test(Circle (2D-Point 3 (+ 5 5)) 8) (Circle(2D-Point 3 10) 8))
(test(Square (2D-Point 4  8) 5) (Square(2D-Point 4  8) 5))
(test(Circle (2D-Point (sqrt 6) (sqrt 2)) 4) (Circle(2D-Point (sqrt 6) (sqrt 2)) 4))
(test(Rectangle (2D-Point 1 5) 5 6 ) (Rectangle(2D-Point 1 5) 5 6))

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

(define ar (MArray 5 '(0 0 0 0 0)))
(test(setvalueA ar 1 4)  '(0 4 0 0 0))
 (define tar (MArray 6 '(0 0 0 0 0)))
(test(setvalueA tar 0 7)  '(7 0 0 0 0))
 (define jar (MArray 7 '(0 0 0 0 0)))
(test(setvalueA jar 3 12)  '(0 0 0 12 0))
 (define tmar (MArray 8 '(0 0 0 0 0)))
(test(setvalueA tmar 0 7)  '(7 0 0 0 0))


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
         
         
         (test(printML(MEmpty)) "[]")
(test(printML(MCons 8(MEmpty))) "[8]")
(test(printML(MCons 5 (MCons 2 (MEmpty))))  "[5, 2]")
(test(printML (MCons 7 (MCons 4 (MCons 10 (MCons 5 (MEmpty)))))) "[7, 4, 10, 5]")




;4-ConcatMl
(define (concatML ml1 ml2)
  (if(MEmpty? ml1)
     ml2
      (MCons (MCons-n ml1) (concatML (MCons-rest ml1) ml2))))

(test(concatML (MCons 7 (MCons 4(MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4(MCons 1(MEmpty)))))
(test(concatML (MCons 9 (MCons 4(MEmpty))) (MCons 1(MCons 2 (MEmpty)))) (MCons 9 (MCons 4(MCons 1(MCons 2(MEmpty))))))
(test(concatML (MCons 1 (MCons 7 (MCons 6(MEmpty)))) (MCons 1 (MCons 0 (MCons 5 (MEmpty))))) (MCons 1 (MCons 7(MCons 6(MCons 1 (MCons 0 (MCons 5 (MEmpty))))))))
(test(concatML (MCons 6 (MCons 3 (MEmpty))) (MCons 0 (MCons 1 (MCons 5 (MEmpty))))) (MCons 6 (MCons 3(MCons 0 (MCons 1 (MCons 5 (MEmpty)))))))



;5-lengthML
(define (lengthML ml)
  (if(MEmpty? ml)
     0
     (+ 1 (lengthML (MCons-rest ml)))))
     
(test(lengthML (MEmpty)) 0)
(test(lengthML (MCons 3 (MCons 2 (MEmpty)))) 2)
(test(lengthML (MCons 3 (MCons 2 (MCons 4 (MEmpty))))) 3)
(test(lengthML (MCons 3 (MCons 2 (MCons 7 (MCons 0 (MEmpty)))))) 4)

;6.-mapML

(define (mapML fun list)
  (cond
    [(MEmpty? list) (MEmpty)]
    [(MCons (fun (MCons-n list)) (mapML fun (MCons-rest list)))]))

(test (mapML add1 (MCons 7 (MCons 4 (MEmpty)))) (MCons 8 (MCons 5 (MEmpty))))
(test (mapML (lambda (x) (* x x)) (MCons 10 (MCons 3 (MEmpty)))) (MCons 100 (MCons 9 (MEmpty))))
(test (mapML (lambda (x) (+ x x)) (MCons 12 (MCons 1 (MEmpty)))) (MCons 24 (MCons 2 (MEmpty))))
(test (mapML (lambda (x) (- x x)) (MCons 12 (MCons 1 (MEmpty)))) (MCons 0 (MCons 0 (MEmpty))))


;7.-Filter

(define (filterML predicate list)
  (cond
    [(MEmpty? list) (MEmpty)]
    [(eq? (predicate (MCons-n list)) #t) (MCons (MCons-n list) (filterML predicate (MCons-rest list)))]
    [else (filterML predicate (MCons-rest list))]))

(test (filterML (lambda (x) (not (zero? x))) (MCons 2 (MCons 0 (MCons 1 (MEmpty))))) (MCons 2 (MCons 1 (MEmpty))))
(test(filterML (lambda (l) (not (MEmpty? l)))
(MCons (MCons 1 (MCons 4 (MEmpty))) (MCons (MEmpty) (MCons 1 (MEmpty)))))
     (MCons (MCons 1 (MCons 4 (MEmpty))) (MCons 1 (MEmpty))))

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

(define radio 6378)
(define Dlat 0)
(define Dlong 0)
(define s 0)
(define x 0)
(define (distance lat1 long1 lat2 long2) 
   (set! Dlat (degrees->radians (- lat1 lat2)))
   (set! Dlong (degrees->radians (- long1 long2)))
   (set! s (+ (expt (sin (/ Dlat 2)) 2) (* (* (cos lat1) (cos lat1)) (expt (sin (/ Dlong 2)) 2))))
   (set! x (* 2 (atan (sqrt s)(sqrt (- 1 s)))))
   (* radio x)
  )

;8.-haversine
(define (haversine loc1 loc2)
  (type-case Coordinates loc1
  (GPS (lat1 long1)
   (type-case Coordinates loc2
     (GPS (lat2 long2) (distance lat1 long1 lat2 long2))))))

;9.-gps-coordinates
(define (gps-coordinates lstplaces)
  (cond
    [(MEmpty? lstplaces) (MEmpty)]
    [(MCons (GPS (GPS-lat (building-loc (MCons-n lstplaces))) (GPS-long (building-loc (MCons-n lstplaces)))) (gps-coordinates (MCons-rest lstplaces)))]))

(test (gps-coordinates (MEmpty)) (MEmpty))
(test (gps-coordinates plazas)(MCons (GPS 19.510482 -99.23411900000002) (MCons (GPS 19.304135 -99.19001000000003) (MEmpty))))

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
    
     ))))|#





