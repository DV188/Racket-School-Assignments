; COMP 3007 - Assignment 3
; Saturday, November 22, 2014
; Danen Van De Ven, 100820351

#lang racket

; Question 3

; (a)

; Delay from the course notes.
(define-syntax delay
  (syntax-rules ()
    ((_ a ...)
     (lambda () a ...))))

; Force from course notes.
(define (force thunk) (thunk))

; (b)

; Stream-cons from course notes.
(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons x y)
     (cons x (delay y)))))

; (c)

(define (stream-car stream) (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-null? stream) 
  (null? stream))

; Testing stream-car, stream-cdr
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 0))

; Testing stream-null?
(display "integers-starting-from 15 null?: ") (stream-null? (integers-starting-from 15))
(display "empty list null?: ") (stream-null? '())
(newline)

(define (first n s)
  (if (= 0 n) 
      '() 
      (stream-cons (stream-car s) (first (- n 1) (stream-cdr s)))))

(define the-empty-stream '())

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (stream-cons (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-first n s)
  (stream-for-each (lambda (x) (display x) (display " ")) (first n s)))

; Testing stream-ref
(display "5th value of integers: ")(stream-ref integers 5)
(display "1000000th value of integers: ")(stream-ref integers 1000000)
(display "2nd value of integers starting from 15: ") (stream-ref (integers-starting-from 15) 2)
(newline)

; Testing stream-map
(display "First 5 integers squared: ") (display-first 5 (stream-map (lambda (x) (* x x)) integers))
(display "First 10 integers cubed: ") (display-first 10 (stream-map (lambda (x) (* x x x)) integers))
(newline)

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; Testing stream-filter
(display "First 5 odd integers: ") (display-first 5 (stream-filter odd? integers))
(display "First 8 even squared integers: ")
(display-first 8 (stream-filter even? (stream-map (lambda (x) (* x x)) integers)))
(newline)

(define (list->stream lis)
  (if (null? lis)
      the-empty-stream
      (stream-cons (car lis) (list->stream (cdr lis)))))

(define (stream->list str)
  (if (stream-null? str)
      '()
      (cons (stream-car str)
            (stream->list (stream-cdr str)))))

; Testing list->stream, stream->list
(display "list '(1 2 3 4 5) to stream: ") (list->stream '(1 2 3 4 5))
(display "stream back into list: ") (stream->list (list->stream '(1 2 3 4 5)))
(newline)

; (d)

(define ones (stream-cons 1 ones))
(define even-integers (stream-filter (lambda (x) (even? x)) integers))
(define (random-numbers n)
  (cons (random n) (delay (random-numbers n))))

(define (sieve stream)
  
  (define (divisible? x y) (= (remainder x y) 0))
  (define no-sevens
    (stream-filter (lambda (x) (not (divisible? x 7)))
                   integers))
  
  (stream-cons
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

; Testing

(display "First 10 ones: ") (display-first 10 ones)
(display "First 10 even-integers: ") (display-first 10 even-integers)
(display "First 10 random: ") (display-first 10 (random-numbers 100))
(display "First 10 random: ") (display-first 10 (random-numbers 100))
(display "First 10 primes: ") (display-first 10 primes)
(newline)

; (e)

(define (partial-sums pos-ints)
  (let ((n (stream-car pos-ints)))
    (stream-cons (/ (* n (+ n 1)) 2)
               (partial-sums (stream-cdr pos-ints)))))

; Testing partial-sums
(display "Partial sums, 1 3 6 10 15 21 28 36 45 55: ") (display-first 10 (partial-sums integers))































