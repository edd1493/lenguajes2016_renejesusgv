#lang plai

(require "practica3-base.rkt")

;1.-zones
(define (zones rest max)
  (let ([range (- max rest)])
    (list (resting rest (+ rest (- (* range (+ 0.5 (* 0.1 0))) 1)))
          (warm-up (+ rest (* range (+ 0.5 (* 0.1 0)))) (+ rest (-(* range (+ 0.5 (* 0.1 (+ 0 1)))) 1)))
          (fat-burning (+ rest (* range (+ 0.5 (* 0.1 1)))) (+ rest (-(* range (+ 0.5 (* 0.1 (+ 1 1)))) 1)))
          (aerobic (+ rest (* range (+ 0.5 (* 0.1 2)))) (+ rest (-(* range (+ 0.5 (* 0.1 (+ 2 1)))) 1)))
          (anaerobic (+ rest (* range (+ 0.5 (* 0.1 3)))) (+ rest (-(* range (+ 0.5 (* 0.1 (+ 3 1)))) 1)))
          (maximum (+ rest (* range (+ 0.5 (* 0.1 4)))) max))))

(test(zones 70 120)
     (list
     (resting 70 94.0)
     (warm-up 95.0 99.0)
     (fat-burning 100.0 104.0)
     (aerobic 105.0 109.0)
     (anaerobic 110.0 114.0)
     (maximum 115.0 120)))

(test(zones 70 100)
     (list
     (resting 70 84.0)
     (warm-up 85.0 87.0)
     (fat-burning 88.0 90.0)
     (aerobic 91.0 93.0)
     (anaerobic 94.0 96.0)
     (maximum 97.0 100)))

(test(zones 80 190)
     (list
     (resting 80 134.0)
     (warm-up 135.0 145.0)
     (fat-burning 146.0 156.0)
     (aerobic 157.0 167.0)
     (anaerobic 168.0 178.0)
     (maximum 179.0 190)))

(test(zones 60 150)
     (list
     (resting 60 104.0)
     (warm-up 105.0 113.0)
     (fat-burning 114.0 122.0)
     (aerobic 123.0 131.0)
     (anaerobic 132.0 140.0)
     (maximum 141.0 150)))

(test(zones 80 160)
     (list
     (resting 80 119.0)
     (warm-up 120.0 127.0)
     (fat-burning 128.0 135.0)
     (aerobic 136.0 143.0)
     (anaerobic 144.0 151.0)
     (maximum 152.0 160)))
          
;se define my-zones para los sig ejemplos
(define my-zones (zones 50 180))

;función auxiliar que devuelve el tipo de un hrz
(define (type-of hrz)
  (cond
    [(resting? hrz) 'resting]
    [(warm-up? hrz) 'warm-up]
    [(fat-burning? hrz) 'fat-burning]
    [(aerobic? hrz) 'aerobic]
    [(anaerobic? hrz) 'anaerobic]
    [(maximum? hrz) 'maximum]))

;2.-get-zone
(define (get-zone smb list-zones)
  (cond
    [(empty? list-zones) '()]
    [(eq? smb (type-of (car list-zones))) (car list-zones)]
    [else (get-zone smb (cdr list-zones))]))

(test (get-zone 'anaerobic my-zones) (anaerobic 154.0 166.0))
(test (get-zone 'maximum my-zones) (maximum 167.0 180))
(test (get-zone 'resting my-zones) (resting 50 114.0))
(test (get-zone 'aerobic my-zones) (aerobic 141.0 153.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))

(define (between? n1 n2 n3)
  (cond
    [(and (>= n1 n3) (<= n2 n3)) #t]
    [else #f]))

(define (aux? hrz n)
  (type-case HRZ hrz
    (resting (low high) (between? high low n))
    (warm-up (low high) (between? high low n))
    (fat-burning (low high) (between? high low n))
    (aerobic (low high) (between? high low n))
    (anaerobic (low high) (between? high low n))
    (maximum (low high) (between? high low n))))

;3.-bpm->zone
(define (bpm->zone list-frec list-zones)
  (cond
    [(or (empty? list-frec) (empty? list-zones)) '()]
    ;[(aux (car list-zones) (car list-frec)) (list (car list-zones) (bpm->zone (cdr list-frec) list-zones))]
    ;[else (bpm->zone (cdr list-frec) (cdr list-zones))]))
    [else (cons (car (filter (lambda (x) (aux? x (car list-frec))) list-zones)) (bpm->zone (cdr list-frec) list-zones))]))

(test (bpm->zone empty my-zones) '())
(test (bpm->zone '(50 60) my-zones)
      (list (resting 50 114.0) (resting 50 114.0)))
(test (bpm->zone '(140 141) my-zones)
      (list (fat-burning 128.0 140.0) (aerobic 141.0 153.0)))
(test (bpm->zone '(50 115 180) my-zones)
      (list (resting 50 114.0) (warm-up 115.0 127.0) (maximum 167.0 180)))
(test (bpm->zone '(120) my-zones) (list (warm-up 115.0 127.0)))

    





;-----------------------------------------------------------------------------------------------------------------------------------------------------------------
;sección 2

;1.- ninBT
(define (ninBT ab)
  (type-case BTree ab
    [EmptyBT() 0]
    [BNode(c l e r)
          (if(and(EmptyBT? l)(EmptyBT? r))
             0
             (+(+ (ninBT l) 1) (ninBT r)))]))

(test(ninBT (EmptyBT)) 0)
(test(ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)

;2.-
(define (nlBTAux ab)
  (type-case BTree ab
    [EmptyBT() 0]
    [BNode(c l e r) 
          (+ (+ (nlBTAux l) 1) (nlBTAux r))]))

(define (nlBT ab)
  (- (nlBTAux ab) (ninBT ab)))

(test(nlBT (EmptyBT))0)
(test(nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 2)


;3.-
(define (nnBT ab)
  (type-case BTree ab
    [EmptyBT() 0]
    [BNode(c l e r) 
          (+ (+ (nnBT l) 1) (nnBT r))]))

(test(nnBT (EmptyBT)) 0)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)



;4.-

(define (mapBT f ab)
  (if(EmptyBT? ab)
     ab
     (BNode (BNode-c ab) (mapBT f (BNode-l ab)) (f(BNode-e ab)) (mapBT f (BNode-r ab)))))

(test (mapBT add1 (EmptyBT)) (EmptyBT))
(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))
(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))

;mconcat
(define (mconcat l1 l2)
  (if(empty? l1)l2
     (cons (car l1) (mconcat (cdr l1) l2))))



;5.-PREORDER
(define (preorderBT ab)
  (if(EmptyBT? ab)
     empty
     (cons (BNode-e ab) (mconcat (preorderBT (BNode-l ab)) (preorderBT (BNode-r ab))))))



;(preorderBT arbol-base)
;inorder
(define (inorderBT ab)
  (if(EmptyBT? ab)
     empty
     (mconcat (inorderBT (BNode-l ab)) (cons (BNode-e ab) (inorderBT (BNode-r ab))))))

;(inorderBT arbol-base)
;posorder
(define (posorderBT ab)
  (if(EmptyBT? ab)
     empty
      (mconcat (posorderBT (BNode-l ab)) (mconcat (posorderBT (BNode-r ab)) (list (BNode-e ab))))))

;(posorderBT arbol-base)



