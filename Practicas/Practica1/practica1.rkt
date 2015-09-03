#lang plai

#|Section 1|#

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

;función auxiliar que crea una lista hasta el entero n
(define (until n)
  (cond
    [(= n 0) '()]
    [else (cons n (until (- n 1)))]))
;función auxiliar que busca divisores de un número n
(define (criba list n)
  (cond
    [(empty? list) '()]
    [(integer? (/ n (car list))) (cons (car list) (criba (cdr list) n))]
    [else (criba (cdr list) n)]))
;función auxiliar que crea una lista de divisores
(define (listdiv n)
  (criba (until n) n))

;función auxiliar que dice si un número es primo
(define (prime? x)
  (= 2 (length (listdiv x))))

(define (primes x)
  (cond
    [(= x 1) '()]
    [(prime? x) (cons x (primes (- x 1)))]
    [else (primes (- x 1))]))

(test (primes 30) '(29 23 19 17 13 11 7 5 3 2))
(test (primes 15) '(13 11 7 5 3 2))
(test (primes 11) '(11 7 5 3 2)) 
(test (primes 4) '(3 2)) 
(test (primes 1) '()) 

(define (zip list1 list2)
  (if (or (empty? list1) (empty? list2))
      empty
      (cons (list (car list1) (car list2)) (zip (cdr list1) (cdr list2)))))

(test (zip '(0) '()) '())
(test (zip '(0 6 4 0) '(4 5 3)) '((0 4)(6 5)(4 3)))
(test (zip '(5) '(1 2 3 4 5 6 7)) '((5 1)))
(test (zip '(a) '(a v c)) '((a a)))
(test (zip '(a b c d) '(1 2)) '((a 1)(b 2)))

(define reduce (lambda (function list)
                 (if (eq? (cdr list) empty) (car list)
                 (function (car list) (reduce function (cdr list))))))

(test (reduce POW '(2 2 2)) 16)
(test (reduce + '(2 5 3 6 7)) 23)
(test (reduce + '(65)) 65)
(test (reduce - '(4 5)) -1)
(test (reduce * '(1 2 3 4 5)) 120)


#|Section 2|#

(define (mconcat l1 l2)
  (if(empty? l1)l2
     (cons (car l1) (mconcat (cdr l1) l2))))

(test (mconcat '(1234)'(2345))'(1234 2345))
(test (mconcat '(123) '(2223)) '(123 2223))
(test (mconcat '() '()) '())
(test (mconcat '(1) '()) '(1))
(test(mconcat '(2222222) '(333333)) '(2222222 333333))


(define (mmap function list)
  (if(empty? list)empty
     (cons (function (car list)) (mmap function (cdr list)))))

(test (mmap add1 '(3 4 2 3 4 5)) '(4 5 3 4 5 6))
(test (mmap sub1 '(3 4 2 3 4 5)) '(2 3 1 2 3 4))
(test (mmap car '((5 6 4 3))) '(5))
(test (mmap cdr '((4 5 3 9 7 5 5))) '((5 3 9 7 5 5)))
(test (mmap cdr '()) '())


(define (mfilter predicate list)
  (cond
    [(empty? list) empty]
    [(eq? (predicate (car list)) #t) (cons (car list) (mfilter predicate (cdr list)))]
    [else (mfilter predicate (cdr list))]))

(test (mfilter (lambda (x) (not (zero? x)))'(3 3 4 2 0 4 0 34 0)) '(3 3 4 2 4 34))
(test (mfilter (lambda (list) (not (empty? list)))'((4 3 4) ())) '((4 3 4)))
(test (mfilter (lambda (n) (zero? n)) '(0 0 0)) '(0 0 0))
(test (mfilter (lambda (x) (not(= x 4)))'(1 2 3 4 5 6 7 8 9)) '(1 2 3 5 6 7 8 9))
(test (mfilter (lambda (x) (negative? x))'(2 1 0 -1 -2 -3 -4 -5)) '(-1 -2 -3 -4 -5))

(define (any? predicate? list)
  (if (empty? list) #f
  (or (predicate? (car list)) (any? predicate? (cdr list)))))

(test (any? number? '(5 6 4 3 3)) #t) 
(test (any? number? '(g d 2 c x s)) #t) 
(test (any? number? '(d f c !)) #f) 
(test (any? symbol? '(v y 34d c)) #t) 
(test (any? symbol? '(4 4 4 4)) #f) 

(define (every? predicate? list)
  (if (empty? list) #t
  (and (predicate? (car list)) (every? predicate? (cdr list)))))

(test (every? empty? '(5 6 4 3 3)) #f) 
(test (every? number? '(5 6 4 3 4 5 3)) #t) 
(test (every? number? '(d fc 3 4)) #f) 
(test (every? symbol? '(df cf 344d cr)) #t) 
(test (every? symbol? '(4 4 4 4)) #f) 
