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
#|4.zip - Dadas dos listas, regresar una cuyos elementos son listas de tamaño 2, tal que para la i-ésima lista, el primer
elemento es el i-ésimo de la primera lista original, y el segundo elemento es el i-ésimo de la segunda lista original,
si una lista es de menor tamaño que la otra, la lista resultante es del tamaño de la menor, y si una de las listas es vacía,
regresa una lista vacía|#
(define (zip l1 l2)
  (if (or (empty? l1) (empty? l2))
      empty
      (cons (list (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

#|5.reduce - Dada una función de aridad 2 y una lista de n elementos, regresar la evaluación de la funcion
encadenada de todos los elementos|#
(define reduce (lambda (f l)
                 (if (eq? (cdr l) empty)
                 (car l)
                 (f (car l) (reduce f (cdr l))))))



#|SECCIÓN 2|#

#|1.mconcat - Dadas dos listas, regresa la concatenación de la primera con la segunda.|#
(define (mconcat l1 l2)
  (if(empty? l1)l2
     (cons (car l1) (mconcat (cdr l1) l2))))

(test (mconcat '(1234)'(2345))'(1234 2345))
(test (mconcat '(123) '(2223)) '(123 2223))
(test(mconcat '() '()) '())
(test(mconcat '(1) '()) '(1))
(test(mconcat '(2222222) '(333333)) '(2222222 333333))


#|2.mmap - Dada una función de aridad 1 y una lista, regresar una lista con la aplcicación de la función a cada 
uno de los elementos de la lista original|#
(define (mmap f l)
  (if(empty? l)empty
     (cons (f (car l)) (mmap f (cdr l)))))

#|3.mfilter - Dado un predicado de un argumento y una lista, regresa la lista original sin los elementos que al aplicar
el predicado, regrese falso|#
(define (mfilter p l)
  (cond
    [(empty? l) empty]
    [(eq? (p (car l)) #t) (cons (car l) (mfilter p (cdr l)))]
    [else (mfilter p (cdr l))]))