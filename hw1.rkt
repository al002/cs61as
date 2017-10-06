#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed

(define (dupls-removed sent)
  (cond ((empty? sent) '())
        ((member? (first sent) (bf sent)) (dupls-removed (bf sent)))
        (else (se (first sent) (dupls-removed (bf sent)))))
)

; Exercise 2 - Define count-word

(define (count-word sent wd)
  (cond ((empty? sent) 0)
        ((equal? (first sent) wd) (+ 1 (count-word (bf sent) wd)))
        (else (+ 0 (count-word (bf sent) wd))))
)

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Infinite loop, pigl always get evaluate

|#

; Exercise 4 - Define squares

(define (squares sent)
  (if (empty? sent)
      '()
      (se (square (first sent)) (squares (bf sent))))
)

; Exercise 5 - Define switch
(define (switch-it sent)
  (cond ((empty? sent) '())
        ((equal? 'you (first sent)) (se 'me (switch-it (bf sent))))
        ((member? (first sent) '(me I)) (se 'you (switch-it (bf sent))))
        (else (se (first sent) (switch-it (bf sent)))))
)

(define (switch sent)
  (cond ((empty? sent) '())
        ((equal? 'you (first sent)) (se 'I (switch-it (bf sent))))
        (else (switch-it sent)))
)

; Exercise 6 - Define ordered?

(define (ordered? sent)
  (if (or (empty? sent) (= 1 (count sent)))
      true
      (and (< (first sent) (first (bf sent))) (ordered? (bf sent))))
)

; Exercise 7 - Define ends-e

(define (ends-e sent)
  (cond ((empty? sent) '())
        ((equal? 'e (last (first sent))) (se (first sent) (ends-e (bf sent))))
        (else (ends-e (bf sent))))
)

; Exercise 8

#|

as speical form will avoid unnecessary evaluation

|#
