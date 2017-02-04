; COMP 3007 - Assignment 2
; Friday, October 24, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 5

; a) Define a recursive procedure (repeated f n) that applies the 
; function f n times to an argument. 

; E.g. ((repeated sqr 3) 2) returns 256., i.e (sqr(sqr(sqr 2)))

; Repeats a procedure n times.
(define (repeated f n)
  (if (= n 0)
    identity
    (compose (repeated f (- n 1)) f)))

(display "(sqr(sqr(sqr 2))) = ") ((repeated sqr 3) 2)
(display "(sqrt(sqrt(sqrt 256))) = ") ((repeated sqrt 3) 256)
(display "(sqr(sqr 200)) = ") ((repeated sqr 2) 200)
(display "(sqrt(sqrt 1600000000)) = ") ((repeated sqrt 2) 1600000000)
(display "(sqr(sqr(sqr(sqr(sqr 1.4))))) = ") ((repeated sqr 5) 1.4)
(display "(sqrt(sqrt(sqrt(sqrt(sqrt 47434.807416749696))))) = ") ((repeated sqrt 5) 47434.807416749696)
(newline)

; b) Rewrite the above procedure as an iterative process. 

; Iterative repeated procedure.
(define (repeated-iter f n)
  (if (= n 0)
    identity
    (lambda (x) ((repeated-iter f (- n 1)) (f x)))))

(display "(sqr(sqr(sqr 2))) = ") ((repeated sqr 3) 2)
(display "(sqrt(sqrt(sqrt 256))) = ") ((repeated sqrt 3) 256)
(display "(sqr(sqr 200)) = ") ((repeated sqr 2) 200)
(display "(sqrt(sqrt 1600000000)) = ") ((repeated sqrt 2) 1600000000)
(display "(sqr(sqr(sqr(sqr(sqr 1.4))))) = ") ((repeated sqr 5) 1.4)
(display "(sqrt(sqrt(sqrt(sqrt(sqrt 47434.807416749696))))) = ") ((repeated sqrt 5) 47434.807416749696)




