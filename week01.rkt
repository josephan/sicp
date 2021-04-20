; Question 1. Exercise 1.6

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; sqrt-iter using special form, if
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

; sqrt-iter2 using ordinary procedure, new-if 
(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x) x)))

; When Alyssa attempts to compute square roots with `new-if` instead of `if`,
; the procedure gets stuck in an infinite recursion.
; This is because `new-if` uses applicative order while `if` uses normal order.
; `if` is a special form where in (if predicate if-clause else-clause)
; the predicate is evaluated first then only one between if-clause and else-clause is evaluated


; Question 2. squares procedure
(define (squares numbers)
  (squares-iter '() numbers))

(define (squares-iter result numbers)
  (if (null? numbers)
      result
      (squares-iter
        (se result (square (first numbers)))
        (bf numbers))))

; Question 3. switch procedure
(define (switch words)
  (switch-iter '() words))

(define (switch-iter result words)
  (if (null? words)
      result
      (switch-iter
        (se result (switch-word (first words)))
        (bf words))))

(define (switch-word wrd)
  (cond ((equal? wrd 'You) 'I)
        ((equal? wrd 'you) 'me)
        ((or (equal? wrd 'I) (equal? wrd 'me)) 'you)
        (else wrd)))

; Question 4. ordered predicate
(define (ordered? numbers)
  (cond ((null? numbers) #t)
        ((null? (bf numbers)) #t) 
        ((> (first numbers) (first (bf numbers))) #f)
        (else (ordered? (bf numbers)))))

; Question 5. ends-e procedure
(define (ends-e words)
  (ends-e-iter '() words))

(define (ends-e-iter result words)
  (if (null? words)
      result
      (ends-e-iter
        (se result (check-ends-e (first words)))
        (bf words))))

(define (check-ends-e wrd)
  (if (equal? (last wrd) 'e)
      wrd
      '()))

; Question 6. Devise a test to determine if Scheme's and/or are special forms or ordinary functions.
; Why is it advantageous for an interpreter to treat or as a special form?

(define (or-test)
  (or (= 0 0) (or-test)))
; or is a special form because if applicative order evaluation was used,
; then the procedure would be stuck in an infinite recursion

(define (and-test)
  (and (= 0 1) (and-test)))
; or is a special form because if applicative order evaluation was used,
; then the procedure would be stuck in an infinite recursion

; the advantage of treating OR as a special form is that once the first true predicate
; is reached, the remaining predicates don't need to be evaluated
  
      