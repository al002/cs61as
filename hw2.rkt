#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define substitute

(define (substitute sent old-word new-word)
  (cond ((empty? sent) '())
        ((equal? old-word (first sent)) (se new-word (substitute (bf sent) old-word new-word)))
        (else (se (first sent) (substitute (bf sent) old-word new-word))))
)


; Exercise 2 - Try out the expressions!

#|
(lambda (x) (+ x 3))
-> returns: #<procedure>

((lambda (x) (+ x 3)) 7)
-> returns: 10

(define (make-adder num)
  (lambda (x) (+ x num))) 
((make-adder 3) 7)
-> returns: 10

(define plus3 (make-adder 3)) 
(plus3 7)
-> returns: 10

(define (square x) (* x x)) 
(square 5)
-> returns: 25

(define square (lambda (x) (* x x))) 
(square 5)
-> returns 25

(define (try f) (f 3 5)) 
(try +)
-> returns: 8

(try word)
-> returns: 35
|#


; Exercise 3
#|

Number of arguments g has: 0

Type of value returned by g: procedure

|#

; Exercise 4 - Define f1, f2, f3, f4, and f5

(define f1 1)
(define (f2) 2)
(define (f3 x) (+ x 3))
(define (f4) (lambda () 4))
(define (f5) (lambda () (lambda (x) (+ x 5))))

; Exercise 5 - Try out the expressions

(define (t f) 
  (lambda (x) (f (f (f x)))) )

#|
1. ((t add1) 0) returns: 3

2. ((t (t add1)) 0) returns: 9

3. (((t t) add1) 0) returns: 27

|#

; Exercise 6 - Try out the expressions

(define (s x)
  (+ 1 x))

#|

1. ((t s) 0) returns: 3

2. ((t (t s)) 0) returns: 9

3. (((t t) s) 0) returns: 27

|#

; Exercise 7 - Define make-tester

(define (make-tester wd)
  (lambda (x) (equal? x wd))
)

; Exercise 8 - SICP exercises

; SICP 1.31a

(define (product term a next b)
  ; Your code here
  (error "Not yet implemented")
)

(define (estimate-pi)
  ; Your code here
  (error "Not yet implemented")
)

; SICP 1.32a

;; This is called my-accumulate so it doesn't conflict with Simply
;; Scheme's accumulate.
(define (my-accumulate combiner null-value term a next b)
  ; Your code here
  (error "Not yet implemented")
)

;; Write sum in terms of my-accumulate:
(define (sum-accum term a next b)
  ; Your code here
  (error "Note yet implemented")
)

;; Write product in terms of my-accumulate:
(define (product-accum term a next b)
  ; Your code here
  (error "Note yet implemented")
)


; SICP 1.33

(define (filtered-accumulate combiner null-value term a next b pred)
  ; Your code here
  (error "Not yet implemented")
)

(define (sum-sq-prime a b)
  ; Your code here
  (error "Not yet implemented")
)

(define (rel-prime? x y)
  (= (gcd x y) 1))

(define (prod-of-some-numbers n)
  ; Your code here
  (error "Not yet implemented")
)

; SICP 1.40 - Define cubic

(define (cubic a b c)
  ; Your code here
  (error "Not yet implemented")
)

; SICP 1.41 - Define double

(define (double proc)
  ; Your code here
  (error "Not yet implemented")
)

; SICP 1.43 - Define repeated

(define (my-repeated proc n)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 9 - Define my-every

(define (my-every proc sent)
  ; Your code here
  (error "Not yet implemented")
)

; Exercise 10 - Try out the expressions

#|

(every (lambda (letter) (word letter letter)) 'purple)
-> returns:

(every (lambda (number) (if (even? number) (word number number) number))
       '(781 5 76 909 24))
-> returns:

(keep even? '(781 5 76 909 24))
-> returns:

(keep (lambda (letter) (member? letter 'aeiou)) 'bookkeeper)
-> returns:

(keep (lambda (letter) (member? letter 'aeiou)) 'syzygy)
-> returns:

(keep (lambda (letter) (member? letter 'aeiou)) '(purple syzygy))
-> returns:

(keep (lambda (wd) (member? 'e wd)) '(purple syzygy))
-> returns:
|#
