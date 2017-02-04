; COMP 3007 - Assignment 1
; Friday, October 3, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 4: Rewrite it as an iterative process

; Identity and increase functions from class
(define (inc n) (+ n 1))
(define (identity x) x) 

; Generic sum function from example
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Sum-integers helper function
(define (sum-integers a b)
  (sum identity a inc b))

; Iterative summation function that sums the values from a to b
(define (sum-iter term a next b total)
  (if (> a b)
      total
      (sum-iter term (next a) next b (+ total (term a)))))

; Iterative integer summation helper function
(define (sum-integers-iter a b)
  (sum-iter identity a inc b 0))

; Basic test cases
(equal? (sum-integers 1 10) (sum-integers-iter 1 10))
(equal? (sum-integers 99 999) (sum-integers-iter 99 999))
(equal? (sum-integers 34 10000) (sum-integers-iter 34 10000))
(equal? (sum-integers 12 15) (sum-integers-iter 12 15))
(equal? (sum-integers -13 13) (sum-integers-iter -13 13))
(equal? (sum-integers 10 10) (sum-integers-iter 10 10))
(equal? (sum-integers 11 10) (sum-integers-iter 11 10))

