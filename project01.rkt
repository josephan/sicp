; Programming Project 1: Twenty-One

(load "twenty-one.scm")

; 1. Define best-total procedure

(define (best-total cards)
  (define max-points (best-total-iter cards 0))
  (define ace-count (count-aces cards 0))
  (define points (balance-aces max-points ace-count))
   points)

(define (best-total-iter cards total)
  (if (empty? cards)
      total
      (best-total-iter (bf cards) (+ total (card-to-point (first cards))))))

(define (count-aces cards ace-count)
  (if (empty? cards)
      ace-count
      (if (equal? 'a (first (first cards)))
          (count-aces (bf cards) (+ ace-count 1))
          (count-aces (bf cards) ace-count))))

(define (balance-aces points ace-count)
  (if (or (<= points 21) (= ace-count 0))
      points
      (balance-aces (- points 10) (- ace-count 1))))

(define (card-to-point card)
  (let ((sign (first card)))
    (cond ((or (equal? sign 'a) (equal? sign 'A)) 11)
          ((or (equal? sign 'j) (equal? sign 'J)) 10)
          ((or (equal? sign 'q) (equal? sign 'Q)) 10)
          ((or (equal? sign 'k) (equal? sign 'K)) 10)
          (else (bl card)))))

; test results:
;> (best-total '(ad 8s))
;19
;> (best-total '(ad 8s))
;19
;> (best-total '(ad 8s 5h))
;14
;> (best-total '(10s 10h 10s))
;30

; 2. Define a procedure stop-at-17, identical to the dealer's

(define (stop-at-17 customer-hand dealer-up-card)
  (if (<= (best-total customer-hand) 16) #t #f))

; test-results:
;> (stop-at-17 '(ad 8s) null)
;#f
;> (stop-at-17 '(ad 8s 5h) null)
;#t

; 3. Define the procedure play-n
(define (play-n strategy n)
  (play-n-iter strategy n 0))

(define (play-n-iter strategy n win-count)
  (if (= n 0)
      win-count
      (play-n-iter strategy (- n 1) (+ win-count (twenty-one strategy)))))

; test results:
;> (play-n stop-at-17 100)
;-16
;> (play-n stop-at-17 100)
;-14
;> (play-n stop-at-17 100)
;-9
;> (play-n stop-at-17 100)
;-18
;> (play-n stop-at-17 100)
;1

; 4. define a strategy dealer-sensitive
(define (dealer-sensitive customer-hand dealer-up-card)
  (define (dealer-card-of-type? dealer-card card-types)
    (member? (first dealer-card) card-types))
  (define points (best-total customer-hand))
  (or (and (< points 17) (dealer-card-of-type? dealer-up-card '(7 8 9 10 A J K Q a j k q)))
      (and (< points 12) (dealer-card-of-type? dealer-up-card '(2 3 4 5 6))))
  #t
  #f)

; test results:
;> (play-n dealer-sensitive 100)
;-14
;> (play-n dealer-sensitive 100)
;-27
;> (play-n dealer-sensitive 100)
;-35
;> (play-n dealer-sensitive 100)
;-22

; 5. generatlize the function in question 2 to (stop-at n)
(define (stop-at n)
  (lambda (customer-hand dealer-up-card)
    (if (<= (best-total customer-hand) n) #t #f)))

; test results:
;> (play-n (stop-at 17) 100)
;-10
;> (play-n (stop-at 17) 100)
;-7
;> (play-n (stop-at 17) 100)
;-3
;> (play-n (stop-at 17) 100)
;-20

; 6. define a valentine strategy
(define (valentine customer-hand dealer-up-card)
  (let ((points (best-total customer-hand))
        (heart? (has-heart? customer-hand)))
  (cond ((and heart? (< points 19)) #t)
        ((and (not heart?) (< points 17)) #t)
        (else #f))))

(define (has-heart? cards)
  (cond ((empty? cards) #f)
        ((card-is-heart? (first cards)) #t)
        (else (has-heart? (bf cards)))))

(define (card-is-heart? card)
  (or (equal? (last card) 'h) (equal? (last card) 'H)))

; test results:
