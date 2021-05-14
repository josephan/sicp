; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound interval) (car interval))
(define (lower-bound interval) (cdr interval))

; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; Exercise 2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (cond ((= 0 (upper-bound y)) (error "Y upper bound spans 0"))
        ((= 0 (lower-bound y)) (error "Y lower bound spans 0"))
        (else (mul-interval
               x
               (make-interval (/ 1.0 (upper-bound y))
                              (/ 1.0 (lower-bound y)))))))

; Exercise 2.12
(define (make-center-percent center percent-tolerance)
  (make-interval (+ center (* center percent-tolerance))
                 (- center (* center percent-tolerance))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (/ (- (upper-bound i) (center i)) (center i)))

; Exercise 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

; Exercise 2.20
(define (same-parity i . items)
  (if (even? i)
      (cons i (filter even? items))
      (cons i (filter odd? items))))

; Exercise 2.21
(define (square x) (* x x))
(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map square items))

; Exercise 2.22
; Louis' solution reverses the the order of the list
; because the first element in `things` list is being squared
; then inserted into the `answer` list
 
; Exercise 2.23
(define (for-each f items)
  (if (null? items)
      #t
      (begin (f (car items))
             (for-each f (cdr items)))))

; write a procedure, substitute
(define (substitute items old-word new-word)
  (cond ((null? items) null)
        ((equal? (car items) old-word) (append
                                       (list new-word)
                                       (substitute (cdr items) old-word new-word)))
        ((list? (car items)) (cons
                             (substitute (car items) old-word new-word)
                             (substitute (cdr items) old-word new-word)))
        (else (append (list (car items)) (substitute (cdr items) old-word new-word)))))

(define (substitute2 items old-words new-words)
  (cond ((null? items) null)
        ((list? (car items)) (cons
                             (substitute2 (car items) old-words new-words)
                             (substitute2 (cdr items) old-words new-words)))
        ((member? (car items) old-words) (append
                                       (list (item (get-index (car items) old-words) new-words))
                                       (substitute2 (cdr items) old-words new-words)))
        (else (append (list (car items)) (substitute2 (cdr items) old-words new-words)))))

(define (get-index item items)
  (define (get-index-iter item items count)
    (if (equal? item (car items))
        count
        (get-index-iter item (cdr items) (+ count 1))))
  (get-index-iter item items 1))


