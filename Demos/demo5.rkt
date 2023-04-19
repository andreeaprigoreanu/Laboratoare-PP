#lang racket

; evaluare lenesa
; 1 - cu inchideri functionale
(define sum1
  (lambda (x y)
    (lambda () (+ x y))))
(sum1 1 2)
((sum1 1 2))

; 2 - promisiuni (delay, force)
(define sum2
  (lambda (x y)
    (delay (+ x y))))
(sum2 1 2)
(force (sum2 1 2))

(define x (delay (+ 1 2)))
; x
; (force x)
; x
; (force x)
; x
; (force x)

; fluxuri
; 1, 1, 1, ...
(define ones-stream1
  (cons 1 (lambda () ones-stream1)))
(define (my-take stream n)
  (if (zero? n) '()
      (cons (car stream) (my-take ((cdr stream)) (sub1 n)))))

(define ones-stream2
  (cons 1 (delay ones-stream2)))
(define (my-take2 stream n)
  (if (zero? n) '()
      (cons (car stream) (my-take2 (force (cdr stream)) (sub1 n)))))

(define ones-stream
  (stream-cons 1 ones-stream))

;; primește un flux și obține o listă cu primele n elemente din flux
(define (stream-take s n)
  (cond ((zero? n) '())
        ((stream-empty? s) '())
        (else (cons (stream-first s)
                    (stream-take (stream-rest s) (- n 1))))))


