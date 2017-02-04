; COMP 3007 - Assignment 1
; Friday, October 3, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 9

; a) Using Newton's approximation for square root finding as shown
; in the Scheme notes modify the block structured form of the algorithm
; in order to allow a user-defined procedure for good-enough? to be
; passed in the algorithm.   The user-defined procedure should be passed
; into the square-root-finding procedure.

; Approximation of square-root
(define (improve r x)
  (/ (+ r (/ x r )) 2))

; Absolute function to remove negative numbers
(define (absolute x)
  (if (< x 0) (- x ) x))

; Checks when the value is precise enough
(define (good-enough? proc precision)
  (proc precision))

; Loops through values until the precision given is met, applies approximation each loop
(define (sqrt-iterate new-r old-r x precision)
  (if (good-enough? (lambda (x y)
                      (< (absolute (- x y))) 
                      new-r old-r)
                    precision)
      new-r
      (sqrt-iterate (improve new-r x) 
                    new-r
                    x
                    precision)))

; Function call
(define (sqrt x initial precision)
  (sqrt-iterate (improve initial x) initial x precision))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
         (else else-clause)))