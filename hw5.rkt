#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 1
;What are the result of the expressions? Make sure to comment your answer out.
#|
1. (1 2 3 4 5 6)
2. ((1 2 3) . (4 5 6))
3. ((1 2 3) (4 5 6))
|#


; Exercise 2 Mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a. Define left-branch, right-branch, branch-length, and
; branch-structure.

(define (left-branch mobile)
  (list-ref mobile 0))

(define (right-branch mobile)
  (list-ref mobile 1))

(define (branch-length branch)
  (list-ref branch 0))

(define (branch-structure branch)
  (list-ref branch 1))

; b. Define total-weight.

(define (branch-weight branch)
    (cond ((number? (branch-structure branch)) (branch-structure branch))
          (else (total-weight (branch-structure branch)))))

(define (total-weight mobile)
  (define left (left-branch mobile))
  (define right (right-branch mobile))
  (+ (branch-weight left) (branch-weight right)))

; c. Define balanced?

(define (total branch)
  (if (number? (branch-structure branch))
      (* (branch-length branch) (branch-structure branch))
      (if (balanced? (branch-structure branch))
          (* (branch-length branch) (total-weight (branch-structure branch)))
          #f
      )
  )
)

(define (balanced? mobile)
  (equal? (total (left-branch mobile)) (total (right-branch mobile)))
)


; d. Redefine all the necessary procedures to work with the new
; constructors given below.
; Make sure that only one set of constructors is active at any time
; (otherwise Racket will complain about duplicate defintions).

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))


;Exercise 3a - Define square-tree

(define (square-tree d-l)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (square-tree sub-tree)
              (square sub-tree)
          )
       )
  d-l)
)

;Exercise 3b - Define tree-map

(define (tree-map fn tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (tree-map fn sub-tree)
              (fn sub-tree)
          )
       ) 
  tree)
)
              

;Exercise 4 -  Complete the definition of accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (foldr op init (map (lambda (s) (car s)) seqs))
	    (accumulate-n op init (map (lambda (s) (cdr s)) seqs)))))

;Exercise 5 - Complete the definitions of matrix-*-vector, transpose,
; and matrix-*-matrix.

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map "YOUR CODE HERE" m))

(define (transpose mat)
  (accumulate-n "YOUR CODE HERE" "YOUR CODE HERE" mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map "YOUR CODE HERE" m)))


;Exercise 6 - Give the property that op should satisfy:

#|

Your property here

|#

;Exercise 7 - Define equal?

(define (my-equal? l1 l2)
  (cond ((and (empty? l1) (empty? l2)) true)
        ((and (symbol? l1) (symbol? l2)) (eq? l1 l2))
        ((and (pair? l1) (pair? l2)) (and (my-equal? (car l1) (car l2)) (my-equal? (cdr l1) (cdr l2))))
        (else false)))

;Exercise 8 - Complete the definition of subsets
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))


;Exercuse 9 - Modify the calc program

;; Racket calculator -- evaluate simple expressions

; The read-eval-print loop:

(define (calc)
  (display "calc: ")
  (flush-output)
  (print (calc-eval (read)))
  (calc))

; Evaluate an expression:

(define (calc-eval exp)
  (cond ((number? exp) exp)
	((list? exp) (calc-apply (car exp) (map calc-eval (cdr exp))))
	(else (error "Calc: bad expression:" exp))))

; Apply a function to arguments:

(define (calc-apply fn args)
  (cond ((eq? fn '+) (foldr + 0 args)) 
	((eq? fn '-) (cond ((null? args) (error "Calc: no args to -"))
			   ((= (length args) 1) (- (car args)))
			   (else (- (car args) (foldr + 0 (cdr args))))))
	((eq? fn '*) (foldr * 1 args))
	((eq? fn '/) (cond ((null? args) (error "Calc: no args to /"))
			   ((= (length args) 1) (/ (car args)))
			   (else (/ (car args) (foldr * 1 (cdr args))))))
	(else (error "Calc: bad operator:" fn))))
