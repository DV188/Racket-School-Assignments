; COMP 3007 - Assignment 1
; Friday, October 3, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 5

; a) Write a procedure (product term a next b) similar to the summation
; procedure above [question 4]. It should return the product of the values of a function
; at points over a given range.

; Identity and increase functions from class
(define (inc n) (+ n 1))
(define (identity x) x) 

; Linear recursive generic product function
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) ; Multiplication used instead of addition
         (product term (next a) next b))))

; Helper function to call the linear recurive product function given a and b parameters
(define (product-integers a b)
  (product identity a inc b))

; b) Define factorial in terms of product.

; Factorial function using the product function from above
(define (product-fact b)
  (product identity 1 inc b))

(display "Factorial tests:")
(newline)
(display "0! = ") (display (product-fact 0)) (newline)
(display "1! = ") (display (product-fact 1)) (newline)
(display "2! = ") (display (product-fact 2)) (newline)
(display "3! = ") (display (product-fact 3)) (newline)
(display "4! = ") (display (product-fact 4)) (newline)
(display "5! = ") (display (product-fact 5)) (newline)
(display "25! = ") (display (product-fact 25)) (newline) (newline) ; Tested against wolframalpha.com

; c) If your procedure product produces a recursive process,
; write one that produces an iterative process. If it generates an iterative
; process, then write one that generates a recursive process. 

; Iterative solution for the product function
(define (product-iter term a next b total)
  (if (> a b)
      total
      (product-iter term (next a) next b (* total (term a)))))

; Helper function for the product function
(define (product-integers-iter a b)
  (product-iter identity a inc b 1))

; Basic test cases
(display "Product tests:")
(newline)
(equal? (product-integers 1 10) (product-integers-iter 1 10))
(equal? (product-integers 1 100) (product-integers-iter 1 100))
(equal? (product-integers 1 1000) (product-integers-iter 1 1000))
(equal? (product-integers 15 250) (product-integers-iter 15 250))
(equal? (product-integers 5 6) (product-integers-iter 5 6))
(equal? (product-integers 27 234) (product-integers-iter 27 234))
(equal? (product-integers 10000 10001) (product-integers-iter 10000 10001))
(equal? (product-integers 1 3) (product-integers-iter 1 3))
(equal? (product-integers 2 3) (product-integers-iter 2 3))
(equal? (product-integers 3 5) (product-integers-iter 3 5))
(equal? (product-integers 6 1) (product-integers-iter 6 1)) ; Both return 1













