; COMP 3007 - Assignment 1
; Friday, October 3, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 7: Newton's method for cube roots is based on the fact
; that if y is an approximation to the cube root of x, then a better
; approximation is given by the value:

; (x/y^2 + 2y)/3

; Use this formula to implement a cube-root procedure analogous to 
; the square-root procedure from the lecture notes.

(define (cubert x initial precision)
  
  ; Approximation of cube-root
  (define (improve r)
    (/ (+ (/ x (expt r 2)) (* 2 r)) 3)) ; Improve function with substituted cube approximation formula

  ; Absolute function to remove negative numbers
  (define (absolute x)
    (if (< x 0) (- x ) x))
  
  ; Checks when the value is precise enough
  (define (good-enough? p q)
    (< (absolute( - p q)) precision))
  
  ; Loops through values until the precision given is met, applies approximation each loop
  (define (cubert-iterate new-r old-r)
    (if (good-enough? new-r old-r) new-r
        (cubert-iterate (improve new-r) new-r)))
  
  ; Function call
  (cubert-iterate (improve initial) initial))

; Basic test cases
(display "cuberoot(1) = 1: ") (cubert 1 0.1 0.000001)
(display "cuberoot(2) = 1.25992: ") (cubert 2 0.1 0.000001)
(display "cuberoot(3) = 1.44224: ") (cubert 3 0.1 0.000001)
(display "cuberoot(4) = 1.58740: ") (cubert 4 0.1 0.000001)
(display "cuberoot(5) = 1.70997: ") (cubert 5 0.1 0.000001)
(display "cuberoot(8) = 2: ") (cubert 8 0.1 0.000001)
(display "cuberoot(10) = 2.15443: ") (cubert 10 0.1 0.000001)
(display "cuberoot(12) = 2.28942: ") (cubert 12 0.1 0.000001)
(display "cuberoot(15) = 2.46621: ") (cubert 15 0.1 0.000001)
(display "cuberoot(25) = 2.92401: ") (cubert 25 0.1 0.000001)
(display "cuberoot(100000) = 46.41588: ") (cubert 100000 0.1 0.000001)

