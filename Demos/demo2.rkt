#lang racket/gui

; rec pe stiva
(define (sum-list L)
  (if (null? L)
      0
      (+ (car L) (sum-list (cdr L)))))

; rec pe coada
(define (sum-list-tail-helper L acc)
  (if (null? L)
      acc
      (sum-list-tail-helper (cdr L) (+ acc (car L)))))

(define (sum-list-tail L)
  (sum-list-tail-helper L 0))

(define (get-first-even L)
  (cond
    [(null? L) #f]
    [(even? (car L)) (car L)]
    [else (get-first-even (cdr L))]))

; stiva vs coada
(define (add5 L)
  (cond
    [(null? L) '()]
    [else (cons (+ 5 (car L)) (add5 (cdr L)))]))

(define (add5-tail L acc)
  (cond
    [(null? L) acc]
    [else (add5-tail (cdr L) (cons (+ 5 (car L)) acc))]))
