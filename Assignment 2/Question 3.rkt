; COMP 3007 - Assignment 2
; Friday, October 24, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 3: Here is an alternative procedure representation of pairs.
; (define (special-cons x y)
;   (lambda (m) (m x y)))

; a)

; As defined in question.
(define (special-cons x y)
  (lambda (m) (m x y)))

; special-car returns only the first value of the cons pair.
(define (special-car pair)
  (pair (lambda (a b) a)))

; special-cdr returns only the last value of the cons pair.
(define (special-cdr pair)
  (pair (lambda (a b) b)))

(display "'(1 . 2)           --> car: ") (special-car (special-cons 1 2))
(display "                       cdr: ") (special-cdr (special-cons 1 2))
(display "'('a . 'b)         --> car: ") (special-car (special-cons 'a 'b))
(display "                       cdr: ") (special-cdr (special-cons 'a 'b))
(display "'('(1 2) . '(3 4)) --> car: ") (special-car (special-cons '(1 2) '(3 4)))
(display "                       cdr: ") (special-cdr (special-cons '(1 2) '(3 4)))
(newline)

; b)

; This definition of cons, car, and cdr are found in the textbook.
(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1" m))))
  dispatch)

(define (my-car z) (z 0))

(define (my-cdr z) (z 1))

(display "'(1 . 2)           --> car: ") (my-car (my-cons 1 2))
(display "                       cdr: ") (my-cdr (my-cons 1 2))
(display "'('a . 'b)         --> car: ") (my-car (my-cons 'a 'b))
(display "                       cdr: ") (my-cdr (my-cons 'a 'b))
(display "'('(1 2) . '(3 4)) --> car: ") (my-car (my-cons '(1 2) '(3 4)))
(display "                       cdr: ") (my-cdr (my-cons '(1 2) '(3 4)))
(newline)

; c)

; Triple procedure much the same as the question's special-cons
(define (triple x y z)
  (lambda (m) (m x y z)))

(define (first triple)
  (triple (lambda (a b c) a)))

(define (second triple)
  (triple (lambda (a b c) b)))

(define (third triple)
  (triple (lambda (a b c) c)))

(define a (triple 1 2 3))

(display "triple a")
(newline)
(display "first a: ")(first a)
(display "second a: ")(second a)
(display "third a: ")(third a)

















