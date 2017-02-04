; COMP 3007 - Assignment 1
; Friday, October 3, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 3: Write a procedure that computes elements of Pascal's 
; triangle by means of a recursive process. You may create a procedure 
; that returns a single row in the triangle or a number within a particular row

; Iterative factorial function for given n
(define (fact n)

  (define (fact-iter product counter max)
	(if (> counter max)
	  product
	  (fact-iter (* counter product)
				 (+ counter 1)
				 max)))

  (fact-iter 1 1 n))

; Combinatorial function representing n choose r from mathematics
(define (nCr n r)
  (/ (fact n)
	 (* (fact r)
		(fact (- n r)))))

; Outputs the specified row of pascal's triangle
; First row is assumed to be row 0
(define (pascal-row n)

  (define (pascal-row-aux count n)
	(display (nCr n count)) (display " ")
	(if (= count n)
	  (newline)
	  (pascal-row-aux (+ count 1) n)))

  (pascal-row-aux 0 n))

; Outputs pascal's triangle to row n
; 0 based counting is assumed
(define (pascal-triangle n)

  (define (pascal-triangle-aux count n)
	(display "f(") (display count) (display "): ") (pascal-row count)
	(if (= count n)
	  (newline)
	  (pascal-triangle-aux (+ count 1) n)))

  (pascal-triangle-aux 0 n))

; Basic usage examples
(pascal-triangle 15)
(pascal-row 15)






