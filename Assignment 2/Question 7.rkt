; COMP 3007 - Assignment 2
; Friday, October 24, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 7: Write a procedure depth that takes as argument an arbitrarily deeply 
; nested list and returns the maximum depth of any of its sublists; the 
; depth of an object in a list is the number of cars that can be applied 
; to it, not necessarily in a row... So the depth of each member of a 
; given list has to be computed and then the maximum of the results 
; has to be chosen. To compute the depth of each sublist, depth has to 
; recurse. The trivial case is simple: each atomic object has a depth of 
; zero. The depth of a non-atomic object is the depth of the deepest 
; object contained in it plus one.
;
; e.g. (depth 'a) ==> 0
; (depth '(a)) ==> 1
; (depth '(a (b) c)) ==> 2
; (depth '(((((a(((b))))))))) ==> 8 

; Depth of a given tree.
(define (depth tree)
  (begin
    (if (or (null? tree) (not (pair? tree)))
        0
        (max (+ 1 (depth (car tree)))
             (depth (cdr tree))))))

(display "depth 'a = ") (depth 'a)
(display "depth '(a) = ") (depth '(a))
(display "depth '(a (b) c) = ") (depth '(a (b) c))
(display "depth '(((((a(((b)))))))) = ") (depth '(((((a(((b))))))))) 