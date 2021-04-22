; Question 1
; Exercise 1.31(a)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fact n)
  (product (λ (x) x) 1 (λ (x) (+ x 1)) n))

(define (wallis-product-term n)
  (let ((a (+ 0 (* n 2)))
        (b (+ 2 (* n 2)))
        (c (+ 1 (* n 2)))
        (d (+ 1 (* n 2))))
       (/ (* a b) (* c d))))

(define wallis-product
  (product wallis-product-term 1 (λ (x) (+ x 1)) 100))

(define pi-approx (* wallis-product 4.0))

; Exercise 1.32(a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-accumulate term a next b)
  (accumulate + 0 term a next b))

(define (product-accumulate term a next b)
  (accumulate * 1 term a next b))

; Exercise 1.33
(define (filtered-accumulate filter? combiner null-value term a next b)
  (if (> a b)
      null-value
     (combiner
          (if (filter? a) (term a) null-value)
          (filtered-accumulate filter? combiner null-value term (next a) next b))))

(define (prime? n)
  (define (prime-6k? n i)
    (if (> (square i) n)
        #t
        (if (or (= (remainder n i) 0) (= (remainder n (+ i 2)) 0))
            #f
            (prime-6k? n (+ i 6)))))
  (cond ((<= n 3) (> n 1))
        ((or (= (remainder n 2) 0) (= (remainder n 3) 0)) #f)
        (else (prime-6k? n 5))))
  
(define (square x) (* x x))

; a
(define (sum-of-square-of-prime a b)
  (filtered-accumulate prime? + 0 square a (λ (x) (+ x 1)) b))

; b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relative-prime? a b)
  (= (gcd a b) 1))

(define (product-of-relative-prime a)
  (define (filter? x)
    (relative-prime? x a))
  (filtered-accumulate filter? * 1 (λ (x) x) 1 (λ (x) (+ x 1)) a))

; Exercise 1.40
(define (cubic a b c)
  (λ (x) (+ (* x x x) (* a x x) (* b x) c)))

; Exercise 1.41
(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (double proc)
  (lambda (x) (proc (proc x))))

; (((double (double double)) inc) 5) => 21

; Exercise 1.43
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (<= n 1)
      f
      (repeated (compose f f) (- n 1))))

; Exercise 1.46
(define (iterate-improve good-enough? improve-guess)
  (define (iter-improve guess)
    (if (good-enough? guess)
        guess
        (iter-improve (improve-guess guess))))
  iter-improve)

(define (sqrt x)
  ((iterate-improve
   (lambda (guess) (< (abs (- (* guess guess) x)) 0.001))
   (lambda (guess) (/ (+ guess (/ x guess)) 2)))
   1.0))

(define (fixed-point f first-guess)
  ((iterate-improve
    (lambda (guess) (< (abs (- (f guess) guess)) 0.00001))
    (lambda (guess) (f guess)))
    first-guess))