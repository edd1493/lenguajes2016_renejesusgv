#lang plai
#|(test <result-expr> <expected-expr>)|#




(define (POW z w)
  (if (= w 0)
      1
  (* z (POW z (- w 1)))))

(test (POW 1 0) 1)
(test (POW 2 1) 2)
(test (POW 5 5) 3125)
(test (POW 100 20) 10000000000000000000000000000000000000000)
(test (POW 7 7) 823543)    

(define (sum list)
  (cond
    [(empty? list) 0]
    [(empty? (cdr list)) (car list)]
    [else (+ (car list) (sum (cdr list)))]))

(define (average list)
  (cond
    [(empty? list) '()]
    [(= 1 (length list))(sum list)]
    [(/ (sum list) (length list))])) 

(test (average '()) empty)
(test (average '(4 5)) 4.5)
(test (average '(3 5 10)) 6)
(test (average '(10 10 10 10 10 10)) 10)
(test (average '(0 6 4 0)) 2.5)
