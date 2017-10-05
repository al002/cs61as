#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define (describe-time secs)
  (cond ((< secs 60) (se secs 'seconds))
        ((< secs 3600) (se (quotient secs 60) 'minutes (describe-time (remainder secs 60))))
        ((< secs 86400) (se (quotient secs 3600) 'hours (describe-time (remainder secs 3600))))
        (else (se (quotient secs 86400) 'days (describe-time (remainder secs 86400)))))
)

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  (cond ((empty? sent) '())
        ((equal? wd (first sent)) (se '() (bf sent)))
        (else (se (first sent) (remove-once wd (bf sent)))))
)

; Exercise 3 - Define differences
(define (differences nums)
  (cond ((empty? nums) '())
        ((empty? (bf nums)) '())
        (else (se (- (first (bf nums)) (first nums)) (differences (bf nums)))))
)

; Exercise 4 - Define location
(define (location small big)
  (cond ((not (member? small big)) false)
        ((equal? small (first big)) 1)
        (else (+ 1 (location small (bf big)))))
)

; Exercise 5 - Define initials
(define (initials sent)
  (if (empty? sent)
      '()
      (se (first (first sent)) (initials (bf sent))))
)

; Exercise 6 - Define copies
(define (copies num wd)
  (if (= 0 num)
      '()
      (se wd (copies (- num 1) wd)))
)

; Exercise 7 - Define gpa
(define (base-grade g)
  (cond ((equal? 'A (first g)) 4)
        ((equal? 'B (first g)) 3)
        ((equal? 'C (first g)) 2)
        ((equal? 'D (first g)) 1)
        (else 0))
)

(define (grade-modifier g)
  (cond ((equal? '- (last g)) -.33)
        ((equal? '+ (last g)) .33)
        (else 0))
)

(define (grades-sum grades)
  (if (empty? grades)
      0
      (+ (+ (base-grade (first grades)) (grade-modifier (first grades)) (grades-sum (bf grades)))))
)

(define (gpa grades)
  (/ (grades-sum grades) (count grades))
)

; Exercise 8 - Define repeat-words
(define (repeat num wd)
  (if (= num 0)
      '()
      (se wd (repeat (- num 1) wd)))
)

(define (repeat-words sent)
  (cond ((empty? sent) '())
        ((empty? (bf sent)) sent)
        ((number? (first sent)) (se (repeat (first sent) (first (bf sent))) (repeat-words (bf (bf sent)))))
        (else (se (first sent) (repeat-words (bf sent)))))
)

; Exercise 9 - Define same-shape?
(define (same-word-shape? wd1 wd2)
  (if (= (count wd1) (count wd2))
      true
      false)
)

(define (same-shape? sent1 sent2)
  (cond ((and (empty? sent1) (empty? sent2)) true)
        ((and (empty? sent1) (not (empty? sent2))) false)
        ((and (not (empty? sent1)) (empty? sent2)) true)
        ((= (count sent1) (count sent2)) (and (same-word-shape? (first sent1) (first sent2)) (same-shape? (bf sent1) (bf sent2))))
        (else false))
)
