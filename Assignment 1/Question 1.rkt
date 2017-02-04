; COMP 3007 - Assignment 1
; Friday, October 3, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 1

; a) 2*4+(3*5+6*7)*9 = 521
(+ (* 2 4)
   (* (+ (* 3 5)
         (* 6 7))
      9))

; b) -1+(3+4*2-6)*3/(7*2) = 1/14
(+ (- 1)
   (* (+ 3
         (* 4 2)
         (- 6))
      (/ 3
         (* 7 2))))

