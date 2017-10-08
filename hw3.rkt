#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define fast-expt-iter
(define (expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expt-iter (square b) (/ n 2) a))
        (else (expt-iter b (- n 1) (* a b))))
)

(define (fast-expt-iter b n)
  (expt-iter b n 1)
)

; Exericse 2 - Define phi

(define (phi)
  ; Your code here
  (error "Not yet implemented")
)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Exercise 3 - Define cont-frac

;; Recursive version
(define (cont-frac n d k)
  (define (cf i)
    (if (= k i)
        (/ (n k) (d k))
        (/ (n i)
           (+ (d i) (cf (+ i 1.0))))))
  (cf 1)
)

;; Iterative version
(define (cont-frac-iter n d k)
  ; Your code here
  (error "Not yet implemented")
)

(define (e k)
  ; Your code here to estimate e using cont-frac with k terms.
  (error "Not yet implemented")
)

; Exercise 4 - Define next-perf

(define (next-perf n)
  ;  Your code here
  (error "Not yet implemented")
)

; Exercise 5 - Explain what happens when the base cases are interchanged.

#|

Your explanation here

|#

; Exercise 6 - Give a formula relating b, n, counter and product in expt-iter.

#|

Formula for expt:

Formula for expt-iter:

|#
