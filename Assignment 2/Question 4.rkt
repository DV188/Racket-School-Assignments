; COMP 3007 - Assignment 2
; Friday, October 24, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 4

; a) Define a procedure (subsets x) that takes a list as a single argument
; and returns all 2^n subsets of that list, i.e. the power set of the list;

; e.g. (subsets '(a b c)) ==> (() (c) (b) (b c) (a) (a c) (a b) (a b c)) 
; The order of the subsets returned is not important.

(define (subsets set)
  (if (null? set) '(())
      (let ((rest (subsets (cdr set))))
        (append rest
                (map (lambda (subset) (cons (car set) subset))
                     rest)))))

(display "subsets '(a b c): ")(subsets '(a b c)) ;= (() (c) (b) (b c) (a) (a c) (a b) (a b c)) 
(newline)

; b) Define an iterative function (intersection-set set1 set2) that 
; returns the intersection of the two sets. Do not use filter. The sets 
; are represented as unordered lists and you may assume that the lists 
; contain no duplicates. (Order of the elements in the intersection is 
; also not important).

; Returns a list of intesecting values between set1 and set2.
(define (intersection-set set1 set2)
  
  ; Helper function to benifit recursion.
  (define (intersection-set-helper set1 set2 set3)
    
    ; Compares a value to all values of the list to check if in set.
    (define (element-of-set? x set)
      (cond ((null? set) false)
            ((equal? x (car set)) true)
            (else (element-of-set? x (cdr set)))))
    
    ; Iterative method for rebuilding the set with only intersecting values.
    (cond ((or (null? set1) (null? set2)) set3)
          ((element-of-set? (car set1) set2)
           (intersection-set-helper (cdr set1) set2 (cons (car set1) set3)))
          (else (intersection-set-helper (cdr set1) set2 set3))))
  
  (intersection-set-helper set1 set2 '()))

(display "'(a b c d e f g) AND '(e f g h i j): ") (intersection-set '(a b c d e f g) '(e f g h i j))
(display "'(2 4 6 8 10) AND '(10 8 6 4 2): ") (intersection-set '(2 4 6 8 10) '(10 8 6 4 2))
(display "'(a b c 1 2 3) AND '(e f g 4 5 6): ") (intersection-set '(a b c 1 2 3) '(e f g 4 5 6))
(newline)

; c) Using filter, define a procedure (intersection-set set1 set2) that returns the intersection of the
; two lists.  The sets are represented as unordered lists and you may assume that the lists contain no 
; duplicates. (Order of the elements in the intersection is also not important). 

; Intersection procedure utilizing filter and memv to check both lists.
(define (intersection-filter a b)
  (filter (lambda (x) (memv x b)) a))

(display "'(a b c d e f g) AND '(e f g h i j): ") (intersection-filter '(a b c d e f g) '(e f g h i j))
(display "'(2 4 6 8 10) AND '(10 8 6 4 2): ") (intersection-filter '(2 4 6 8 10) '(10 8 6 4 2))
(display "'(a b c 1 2 3) AND '(e f g 4 5 6): ") (intersection-filter '(a b c 1 2 3) '(e f g 4 5 6))
(newline)

; d) Define a procedure (mean lis) that takes any list as argument and 
; returns the arithmetic mean of just the elements that are numbers, 
; ignoring the others. It should signal an error if the list contains no 
; numbers.
;
; e.g. (mean '(1 2 3 4 5)) ==> 3 (mean '(1 a 2 b c d e 3)) ==> 2

; Determines the mean of all the numbers in the list.
; Does some really basic error checking.
(define (mean lis)
  (if (eqv? (filter number? lis) '())
      (error "No numbers in list.")
      (/ (apply + (filter number? lis)) (length (filter number? lis)))))

(display "mean '(1 2 3) = ") (mean '(1 2 3))
(display "mean '(1 a 2 bcde 3) = ") (mean '(1 a 2 bcde 3))
(display "mean '(1 2 3 4 5 a g 6 x e 7 8 9) = ") (mean '(1 2 3 4 5 a g 6 x e 7 8 9))
(display "mean '(a b c): ") (mean '(a b c))

















