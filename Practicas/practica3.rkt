#lang plai

(require "practica3-base.rkt")









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


