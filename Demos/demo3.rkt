#lang racket

; FUNCTII UNCURRY
; cu parametrii impliciti
(define (plus-uncurry x y)
  (+ x y))
(plus-uncurry 2 3)
; cu functie lambda
(define plus-uncurry-lambda
  (lambda (x y)
    (+ x y)))

;Functii curry
; parametrii se trimit pe rand
(define (plus-curry x)
  (lambda (y)
    (+ x y)))
((plus-curry 2) 3)

(define plus-curry2
  (lambda (x)
    (lambda (y)
      (+ x y))))
((plus-curry2 2) 3)

; curry->uncurry
(define (curry->uncurry f)
  (lambda (x y)
    ((f x) y)))
;uncurry->curry
(define (uncurry->curry f)
  (lambda (x)
    (lambda (y)
      (f x y))))

; map
(define L (list 1 2 3 4 5))
(map sub1 L)
(map (lambda (x) (+ 5 x)) L)
(map (plus-curry 5) L)

(map + L L)
(map list L L)

; filter
(filter negative? '(1 2 -4 -6 7 -8))
(filter (lambda (x) (> 5  x)) L)
; functia nu trebuie sa fie neaparat predicat
(filter (lambda (x) x) '(1 2 #t 5 #t 6 #f 6))

; 3: foldl
(foldl (λ (x acc) (+ x acc)) 0 L)
(foldl + 0 L)
(foldl (λ (x y acc) (+ x y acc)) 0 L L)
(foldl + 0 L L)
; reverse list:
(foldl (lambda (x acc) (cons x acc)) '() L)
(foldl cons '() L)
; copierea unei liste:
(foldl (lambda (x acc) (append acc (list x))) '() L)

; 4: foldr
(foldr (λ (x acc) (+ x acc)) 0 L)
(foldr + 0 L)
(foldr (λ (x y acc) (+ x y acc)) 0 L L)
(foldr + 0 L L)
; copierea unei liste:
(foldr (lambda (x acc) (cons x acc)) '() L)
(foldr cons '() L)
; reverse list:
(foldr (lambda (x acc) (append acc (list x))) '() L)

(foldr (λ (x y acc) (append (list (list x y)) acc)) '() '(1 2 3) '(4 5 6))

; map - folosind foldr
(define (my-map f L)
  (foldr (lambda (x acc) (cons (f x) acc)) '() L))
; filter - folosind foldr
(define (my-filter f L)
  (foldr (lambda (x acc)
           (if (f x)
               (cons x acc)
               acc))
         '() L))

; apply
(apply + L)
(apply + 2 3 L)
(apply cons '(1 2))
(apply list L)
(apply list '(2 4 5 6) L)

; transpusa unei matrice
(define matrix '((1 2 3 4) (3 4 7 8) (4 6 3 5)))
(apply map list matrix)



 
