; Question 1
; Exercise 1.16
(define (square x) (* x x))

(define (exp b n) (exp-iter b n 1))

(define (exp-iter b n product)
  (if (= n 0)
      product
      (exp-iter b (- n 1) (* product b))))

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))
; Answer:
(define (fast-exp-iter b n product)
  (cond ((= n 0) product)
        ((even? n) (fast-exp-iter (square b) (/ n 2) product))
        (else (fast-exp-iter b (- n 1) (* product b)))))

; Exercise 1.35
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

(fixed-point (λ (x) (+ 1 (/ 1 x))) 1.0)

; Exercise 1.37
; a) recursive
(define (cont-frac n d k)
  (define (cont-frac-rec n d k count)
    (if (= k count)
      (/ (n k) (d k))
      (/ (n k) (+ (d k) (cont-frac-rec n d k (+ count 1))))))
  (cont-frac-rec n d k 0))

; 10 iterations needed to be accurate within 4 decimal places
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)

; b) iterative
(define (cont-frac2 n d k)
  (cont-frac-iter n d k (/ (n k) (d k))))

(define (cont-frac-iter n d k last_calculation)
  (if (= k 0)
      last_calculation 
      (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) last_calculation)))))

(cont-frac2 (lambda (i) 1.0)
            (lambda (i) 1.0)
            10)

; Exercise 1.38
(define (n-e i) 1.0)
(define (d-e i)
  (if (= 2 (remainder i 3))
      (* 2 (/ (+ i 1) 3.0))
      1.0))

(define euler-number (+ (cont-frac2 n-e d-e 10) 2))
(write euler-number)

; Question 2
(define (next-perf n)
  (if (= (sum-of-factors n) n)
      n
      (next-perf (+ n 1))))

(define (sum-of-factors n)
  (sum (get-factors n) 0))

(define (sum nums i)
  (if (empty? nums)
      i
      (sum (bf nums) (+ i (first nums)))))

(define (get-factors n)
  (define (get-factors-iter n i nums)
    (if (= i n)
        nums
        (if (= (remainder n i) 0)
            (get-factors-iter n (+ i 1) (se nums i))
            (get-factors-iter n (+ i 1) nums))))
  (get-factors-iter n 1 '()))

; (next-perf 29) => 496

; Question 3
; Changing the order of the base case returns a different result when
; the kinds-of-coin is 0 given an amount 0
; original: (cc 0 0) => 1
; modified: (cc2 0 0) => 0

; Question 4 (skipped)

; Extra for experts:
(define (number-of-partitions n)
  (define (find-partitions n i)
    (cond ((= n 0) 1)
          ((or (= i 0) (< n 0)) 0)
          (else (+
                 (find-partitions n (- i 1))
                 (find-partitions (- n i) i)))))
  (find-partitions n n))
