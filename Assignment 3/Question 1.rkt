; COMP 3007 - Assignment 3
; Saturday, November 22, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 1: Sort a list of distinct numbers in ascending order, using the following
; divide-and-conquer strategy (Quicksort): divide the list of numbers into two lists:
; one that contains all items that are strictly smaller than the first item (often
; called the pivot), and another with all those items that are strictly larger than
; the first item. Then the two smaller lists are sorted using the same procedure.
; Once the two lists are sorted, the pieces are juxtaposed.

; For example, given (11 8 14 7) the pivot is 11. We make two lists, (8 7) and (14).
; The second is already sorted; sorting the first - pivot is 8 - yields (7 8).
; Putting the three pieces together: (7 8) 11 (14) ==> (7 8 11 14).

; Quicksorts does the splitting of the list while holding the start of the list in the pivot.
(define (quicksort lis)
  
  ; The split function uses a filter to filter the given list with respect to the given procedure.
  (define (split compare lis)
    (filter compare lis))
  
  (cond ((null? lis) '())
        (else (let ((pivot (car lis)))
                (append (quicksort (split (lambda (x) (< x pivot)) lis))
                        (split (lambda (x) (= x pivot)) lis)
                        (quicksort (split (lambda (x) (> x pivot)) lis)))))))

(display "   Input                     --->         Output") (newline)
(display "--------------------------------------------------------") (newline) (newline)
(display "'(11 8 14 7)                 ---> ")(quicksort '(11 8 14 7)) (newline)
(display "'(9 8 7 6 5 4 3 2 1)         ---> ")(quicksort '(9 8 7 6 5 4 3 2 1)) (newline)
(display "'(1 2 3 4 5 -6 -7 -8 -9)     ---> ")(quicksort '(1 2 3 4 5 -6 -7 -8 -9)) (newline)
(display "'(100 4383 39948 827172 488) ---> ")(quicksort '(100 4383 39948 827172 488)) (newline)
(display "'(0 0 0 0)                   ---> ")(quicksort '(0 0 0 0)) (newline)
(display "'(1 2)                       ---> ")(quicksort '(1 2)) (newline)
(display "'(2 1)                       ---> ")(quicksort '(2 1)) (newline)





