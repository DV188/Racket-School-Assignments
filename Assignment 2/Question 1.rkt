; COMP 3007 - Assignment 2
; Friday, October 24, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 1: An interval is defined by an upper and lower bound. You are 
; required to write the procedures: add-interval, subtract-interval, 
; multiply-interval and divide-interval that add, subtract, multiply 
; and divide two intervals respectively. You should create a make-interval 
; procedure,along with procedures to access the upper and lower bounds.
; You must deal with intervals that span zero.

; Creates and interval with integers x and y.
(define (make-interval x y)
  (cons x y))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define interval1 (make-interval 15 30))
(define interval2 (make-interval 10 12))
(define interval3 (make-interval -5 5))
(define interval4 (make-interval -13 18))

(display "interval 1 --> ") interval1
(display "interval 2 --> ") interval2
(newline)
(display "interval 3 --> ") interval3
(display "interval 4 --> ") interval4
(newline)

; Adds intervals x and y according to question specifications.
(define (add-interval x y)
  (make-interval
   (+ (lower-bound x) (lower-bound y))
   (+ (upper-bound x) (upper-bound y))))

(display "Addition")
(newline)
(display "interval 1 + interval 2: [15,30] + [10,12] = [15 + 10, 30 + 12] --> ")(add-interval interval1 interval2)
(display "interval 3 + interval 4: [-5,5] + [-13,18] = [-5 + -13, 5 + 18] --> ")(add-interval interval3 interval4)
(newline)

; Subtracts intervals x and y according to question specifications.
(define (subtract-interval x y)
  (make-interval
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))))

(display "Subtraction")
(newline)
(display "interval 1 - interval 2: [15,30] - [10,12] = [15 - 12, 30 - 10] --> ")(subtract-interval interval1 interval2)
(newline)
(display "interval 3 - interval 4: [-5,5] - [-13,18] = [-5 - -13, 5 - 18] --> ")(subtract-interval interval3 interval4)
(newline)

; Multiplies intervals x and y according to question specifications.
(define (multiply-interval x y)
  (let ((ac (* (lower-bound x) (lower-bound y)))
        (ad (* (lower-bound x) (upper-bound y)))
        (bc (* (upper-bound x) (lower-bound y)))
        (bd (* (upper-bound x) (upper-bound y))))
    (make-interval
     (min ac ad bc bd)
     (max ac ad bc bd))))

(display "Multiplication")
(newline)
(display "interval 1 * interval 2: [15,30] * [10,12] =
[min(15 * 10, 15 * 12, 30 * 10, 30 * 12), max(15 * 10, 15 * 12, 30 * 10, 30 * 12)] --> ")(multiply-interval interval1 interval2)
(display "interval 3 * interval 4: [-5,5] * [-13,18] =
 [min(-5 * -13, -5 * 18, 5 * -13, 5 * 18), max(-5 * -13, -5 * 18, 5 * -13, 5 * 18)] --> ")(multiply-interval interval3 interval4)
(newline)

; Divides intervals x and y according to question specifications.
(define (divide-interval x y)
  (if (and (< (lower-bound y) 0)
           (> (upper-bound y) 0))
      (display "Interval in denominator contains 0.")
      (multiply-interval x
                         (make-interval (/ 1 (upper-bound y))
                                        (/ 1 (lower-bound y))))))

(display "Division")
(newline)
(display "interval 1 / interval 2: [15,30] / [10,12] = [15,30] * [1/12, 1/15] --> ")(divide-interval interval1 interval2)
(display "interval 3 / interval 4: [-5,5] / [-13,18] = [-5,5] * [1/18, 1/-13] --> ")(divide-interval interval3 interval4)
(newline)



