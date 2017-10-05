#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom
10
;2. Compound Expression (3 Atoms)
(+ 1 1 8)
;3. Compound Expression (4 Atoms)
(+ 2 3 3 2)
;4. Compound Expression (1 Atom and 2 subexpressions)
(+ 3 (+ 1 3) (+ 1 2))
;5. Any Other Kind Expression
(* 5 2)

;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  (word (first wd) (first (bf wd)))
)

;;2. Define two-first
(define (two-first x y)
  (word (first x) (first y))
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  (word (first (first sent)) (first (item 2 sent)))
)

;Exercise 2 - Define teen?
(define (teen? num)
  (and (>= num 13) (<= num 19))
)

;Exercise 3 - Define indef-article
(define (indef-article wd)
  (se (cond ((equal? 'a (first wd)) 'an)
            ((equal? 'i (first wd)) 'an)
            ((equal? 'u (first wd)) 'an)
            ((equal? 'e (first wd)) 'an)
            ((equal? 'o (first wd)) 'an)
            (else 'a))
          wd)
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  (se (bl sent) 'and (last sent))
)

;Exercise 5 - Define query
(define (query sent)
  (se (first (bf sent)) (first sent) (bl (bf (bf sent))) (word (last sent) '?))
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  (cond ((and (= '12 (first time)) (equal? 'am (last time)) 0))
        ((and (= '12 (first time)) (equal? 'pm (last time)) 12))
        ((equal? (last time) 'am) (first time))
        (else (+ 12 (first time))))
)

(define (american-time time)
  (cond ((= time 0) '(12 am))
        ((= time 12) '(12 pm))
        ((< time 12) (se time 'am))
        (else (se (- time 12) 'pm)))
)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  (cond ((< secs 60) (se secs 'seconds))
        ((< secs 3600) (se (/ secs 60.0) 'minutes))
        ((< secs 86400) (se (/ secs 3600.0) 'hours))
        (else (se (/ secs 86400.0) 'days)))
)

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

#|

because parameter name `word` conflict with bulitin procedure name

|# 