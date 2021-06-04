(require sicp-pict)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

; exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

; exercise 2.45
(define (split first-half second-half)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split first-half second-half) painter (- n 1))))
          (first-half painter (second-half smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

; exercise 2.46
;(define make-vect cons)
;(define (xcor-vect vect) (car vect))
;(define (ycor-vect vect) (cdr vect))


;(define (add-vect v1 v2)
;  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
;             (+ (ycor-vect v1) (ycor-vect v2))))

;(define (sub-vect v1 v2)
;  (make-vect (- (xcor-vect v1) (xcor-vect v2))
;             (- (ycor-vect v1) (ycor-vect v2))))

;(define (scale-vect v1 scale)
;  (make-vect (* (xcor-vect v1) scale) (* (ycor-vect v1) scale)))


; exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (car (cdr (cdr frame))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (cdr (cdr frame)))


; exercise 2.48
;(define make-segment cons)
;(define start-segment car)
;(define end-segment cdr)

; exercise 2.49
(define frame-outline
 (list
  (make-segment
   (make-vect 0 0)
   (make-vect 0 1))
  (make-segment
   (make-vect 0 0)
   (make-vect 1 0))
  (make-segment
   (make-vect 1 0)
   (make-vect 1 1))
  (make-segment
   (make-vect 0 1)
   (make-vect 1 1))))

; (paint (segments->painter frame-outline))

(define frame-x
 (list
  (make-segment
   (make-vect 0 0)
   (make-vect 1 1))
  (make-segment
   (make-vect 0 1)
   (make-vect 1 0))))

; (paint (segments->painter frame-x))

(define frame-diamond
 (list
  (make-segment
   (make-vect 0 0.5)
   (make-vect 0.5 1))
  (make-segment
   (make-vect 0.5 0)
   (make-vect 1 0.5))
  (make-segment
   (make-vect 0 0.5)
   (make-vect 0.5 0))
  (make-segment
   (make-vect 1 0.5)
   (make-vect 0.5 1))))

; (paint (segments->painter frame-diamond))

; exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))