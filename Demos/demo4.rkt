#lang racket

(define x 2)
(define a 10)

; ---------------------- SCOPING ---------------------- 
(define (f1 x y) (+ x y))
;(f1 3 3)
; (f1 x 3)
;(f1 x y)

(define (f2 x z) (+ x z))
;(f2 4 4)
;(f2 a 4)
;(f2 1 z)

(define (f3 x y) (+ a x y))
;(f3 1 1)

(define (f4 x y) (f3 a x))
;(f4 1 1)
;(f4 x 1)
;(f4 a a)

;(+ x y) ; y not in scope
;(+ a x)


; ---------------------- LET ----------------------
(define b 2)
(let ([b 3] [a (+ b 1)]) ; (b 3) (a 3)
  (cons a b))            ; (3 . 3)

(define (f x y)
  (+ x y                        ; (x 2) (y 3)
     (let ([x 1] [y (+ x 2)])   ; (x 1) (y 4)
       (+ x y))))               ; 5
(f 2 3)

(let ([a 1])
  (let ([a 2]) (print a)) )
(display "\n")

(let ([a 1])
  (let ([a 2])
    (let ([f (λ () (print a))])   ; (a 2)
      (f))))
(display "\n")

(let ([a 1])
  (let ([f (λ () (print a))])
    (let ([a 2])
      (f))))
(display "\n")


; -------------- LET* --------------------
; (define b 2)
(let ([b 3] [a (+ b 1)]) ; (b 3) (a 3)
  (cons a b))            ; (3 . 3)
(let* ([b 3] [a (+ b 1)]) ; (b 3) (a 4)
  (cons a b))            ; (4. 3)

; ------------ LETREC --------------------
;(letrec ([a (+ b 1)] [b 2])
;  (cons a b))
(letrec ([prod-list (lambda (L)
                      (if (null? L)
                          1
                          (* (car L) (prod-list (cdr L)))))])
  (prod-list '(2 3 4 5)))

; ------------ NAMED LET -----------------

(let prod-list ([L '(2 3 4 5)])
  (if (null? L)
                          1
                          (* (car L) (prod-list (cdr L)))))

; my-map: rec pe coada
(define (my-map-helper f L acc)
  (if (null? L) (reverse acc)
    (my-map-helper f (cdr L) (cons (car L) acc))))

(define (my-map1 f L)
  (my-map-helper f L '()))

; map cu named let
(define (my-map2 f L)
  (let my-map-helper ([L L] [acc '()])
    (if (null? L) (reverse acc)
    (my-map-helper (cdr L) (cons (f (car L)) acc)))))
(my-map2 add1 '(2 3 4 5))
  


; -------- INCHIDERE FUNCTIONALA ---------
(define (func2 x)
  (define func3
    (λ (x y) (+ x y a)))           ; inchidere functionala
  (func3 x 1))                     ; aplicare ; in contextul lui func2 (x 2)
;(func2 2)                         ; aplicare ; in contextul global (a 10)