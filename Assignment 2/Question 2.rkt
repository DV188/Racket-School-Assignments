; COMP 3007 - Assignment 2
; Friday, October 24, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 2: Fill in the missing expressions to complete the following
; definitions of some basic list-manipulation operations as accumulations. (if 
; necessary see the notes and the textbook for definitions of map, 
; append, accumulate.) Make sure your definitions behave properly.

; a)

; Accumulate procedure from the course textbook.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; my-map procedure maps given procedure to each value of sequence.
; Uses the accumulate function to make its way through list.
(define (my-map proc sequence)
  (accumulate (lambda (x y) (cons (proc x) y))
              '()
              sequence))
; Randomly created procedure to test my-map
(define (random-procedure)
  (lambda (x) (+ x (- x (* x (+ x x))))))

(display "my-map --> ") (my-map (lambda (x) (* x x)) '(1 2 3 4))
(display "map    --> ") (map (lambda (x) (* x x)) '(1 2 3 4))
(display "my-map --> ") (my-map (lambda (x) (/ x x)) '(1 2 3 4))
(display "map    --> ") (map (lambda (x) (/ x x)) '(1 2 3 4))
(display "my-map --> ") (my-map (random-procedure) '(1 2 3 4))
(display "map    --> ") (map (random-procedure) '(1 2 3 4))
(newline)

; b)

; my-append uses accumulate to combine the given sequences into a unified list.
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(display "my-append --> ")(my-append '(1 2 3) '(4 5 6))
(display "append    --> ")(append '(1 2 3) '(4 5 6))
(display "my-append --> ")(my-append '('a 'b 'c) '('d 'e 'f))
(display "append    --> ")(append '('a 'b 'c) '('d 'e 'f))
(display "my-append --> ")(my-append '('(1 2) 3 '(4)) '(5 '(6 7) 8))
(display "append    --> ")(append '('(1 2) 3 '(4)) '(5 '(6 7) 8))
(newline)

; c)

; my-length calculates the length of the list using accumulate.
(define (my-length sequence)
   (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(display "my-length --> ")(my-length '(1 2 3 4 5 6))
(display "length    --> ")(length '(1 2 3 4 5 6))
(display "my-length --> ")(my-length '())
(display "length    --> ")(length '())
(display "my-length --> ")(my-length '(1 2 3 4 5 '(6 7 8 9) '(10) 11 12 13 14 15))
(display "length    --> ")(length '(1 2 3 4 5 '(6 7 8 9) '(10) 11 12 13 14 15))
(newline)

; d)

; Maps the procedure to each value of the tree, keeps the tree intact.
(define (treemap proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (proc tree))
        (else (cons (treemap proc (car tree))
                        (treemap proc (cdr tree))))))

(display "treemap")
(newline)
(define l1 '(1 (2 3)((4 5)(6 7))(((8 (9))))))
(display "Initial list: ") l1
(newline)
(display " sqr (expected): '(1 (4 9) ((16 25) (36 49)) (((64 (81)))))")
(newline)
(treemap sqr l1)
(newline)
(display "odd?: ")(treemap odd? l1)
(newline)
(display "even?: ")(treemap even? l1)













