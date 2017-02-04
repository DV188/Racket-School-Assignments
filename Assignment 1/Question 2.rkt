; COMP 3007 - Assignment 1
; Friday, October 3, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 2: A function f is defined by the rule that f(n) = n if n < 4 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) + 4f(n - 4) if n >= 4.

; a)

; Recursive function implemented to meet the requirements above.
(define (f n)
  (if (< n 4)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3)))
         (* 4 (f (- n 4))))))

; b)

; Iterative function implemented to meet the requirements above.
; Uses a helper function to save state at each iteration.
(define (f-iter n)
  (if (< n 4)
      n
      (f-aux 3 2 1 0 3 n)))

(define (f-aux a b c d count n)
  (if (= count n)
      a
      (f-aux (+ a (* 2 b) (* 3 c) (* 4 d)) a b c (+ count 1) n)))

; Print function to simplify the display of both recursive and iterative functions for a specific n.
(define (print n)
  (display "f(") (display n) (display ") -> ")(display "iterative: ")(display (f-iter n))(display " | ")(display "recursive: ")(display (f n))
  (newline))

; Looping print function that loops from 0 to n printing both recursive and iterative results.
(define (printloop n)
  
  (define (printloop-aux count n)
    (print count)
    (if (= count n)
      (display "printloop complete")
      (printloop-aux (+ count 1) n)))
  
  (printloop-aux 0 n))

; Basic test cases.
(f 15)
(f-iter 15)
(print 29)
(newline)
(printloop 10)
  