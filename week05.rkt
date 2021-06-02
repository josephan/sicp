; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; '(1 2 3 4 5 6)
(cons x y)   ; '((1 2 3) 4 5 6)
(list x y)   ; '((1 2 3) (4 5 6))

; Exercise 2.29
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

; a.
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car mobile))
(define (branch-structure branch) (car (cdr branch)))

; b.
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight branch)
  (if (list? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

; c.
(define (balanced-mobile? mobile)
  (and (balanced-torque? mobile)
       (branch-is-number-or-balanced-mobile? (left-branch mobile))
       (branch-is-number-or-balanced-mobile? (right-branch mobile))))

(define (branch-is-number-or-balanced-mobile? branch)
  (if (integer? (branch-structure branch))
      #t
      (balanced-mobile? (branch-structure branch))))

(define (balanced-torque? mobile)
  (= (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

(define (branch-torque branch)
  (* (branch-length branch) (branch-weight branch)))

; d.
; all we need to change is the procedures right-branch and branch-structure to:
(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))

; Exercise 2.30
(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) null)
        ((not (list? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
      
(define (square-tree-map tree)
  (if (integer? tree)
      (square tree)
      (map square-tree-map tree)))

(define tree1 (list 1
                    (list 2 (list 3 4) 5)
                    (list 6 7)))

; Exercise 2.31
(define (tree-map f tree)
  (if (integer? tree)
      (f tree)
      (map (lambda (sub-tree) (tree-map f sub-tree)) tree)))

; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
      

; Exercise 2.36
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
            (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define m1 (list (list 1 -1 2) (list 0 -3 1)))
(define v1 (list 2 1 0))
(matrix-*-vector m1 v1)

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

; Exercise 2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3))  ; 1/6
(fold-right list null (list 1 2 3)) ; '(1 (2 (3 ())))
(fold-left list null (list 1 2 3)) ; '(((() 1) 2) 3)

; (op a b) = (op b a)
; The operation must be commutative for fold-left to equal fold-right.
; In other words the order of the arguments for procedures should not matter 

; Exercise 2.54
(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((and (list? a) (list? b)) (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((eq? a b) #t)
        (else #f)))


