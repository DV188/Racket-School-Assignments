; COMP 3007 - Assignment 2
; Friday, October 24, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 6: Define a procedure rearrange that takes as arguments a possibly 
; nested list of indices and a possibly nested list of items to be 
; rearranged. Your procedure should behave as shown in the following examples:

;(rearrange '(4 3 2 1) '(a b c d)) ==> '(d c b a)
;
;(rearrange ’(4 3 2 1 1 2 3 4) ’(a b c d)) ==> '(d c b a a b c d)
;
;(rearrange '(4 (4 2) 1) '(a b c d)) ==> '(d (d b) a)
;
;(rearrange '(1 2 4 2 3) '(a(b)((c)) d)) ==> '(a (b) d (b) ((c)))
;
;(rearrange ’(4 2) ’(a b)) ==> index out of range error

; Rearranges the letters to correspond to the numbers.
(define (rearrange numbers letters)
  
  ; Produces a list out of a tree.
  ; Flatten was taken from several examples in the textbook.
  (define (flatten tree)
    (cond ((null? tree) '())
          ((pair? tree) (apply append (map flatten tree)))
          (else (list tree))))
  
  (map (lambda (num) (list-ref letters (- num 1)))
       (flatten numbers)))

(rearrange '(4 3 2 1) '(a b c d))
(rearrange '(4 3 2 1 1 2 3 4) '(a b c d))
(rearrange '(4 (4 2) 1) '(a b c d))
(rearrange '(1 2 4 2 3) '(a(b)((c)) d))
(rearrange '(4 2) '(a b))