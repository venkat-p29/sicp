(define (square x) (* x x))

; ---------------------------------
; Exercise 2.2
; ---------------------------------

; Points
(define (make-point x y) (cons x y))

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (dist-points p1 p2)
    (let ((x1 (x-point p1))
          (x2 (x-point p2))
          (y1 (y-point p1))
          (y2 (y-point p2)))
        (sqrt (+ (square (- x2 x1))
                 (square (- y2 y1))))))


; Segments
(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

; Length of a segment
(define (segment-length s)
    (let ((p1 (start-segment s))
          (p2 (end-segment s)))
        (dist-points p1 p2)))


; Mid-point
(define (midpoint-segment s)
    (let* ((p1 (start-segment s))
           (p2 (end-segment s))
           (x1 (x-point p1))
           (x2 (x-point p2))
           (y1 (y-point p1))
           (y2 (y-point p2)))
      (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))


; Print point
(define (print-point p)
    ; (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))



; ---------------------------------
; Exercise 2.3
; ---------------------------------

(define (make-rectangle s1 s2 s3 s4) (cons (cons s1 s2) (cons s3 s4)))

(define (lseg-rect r) (caar r))
(define (bseg-rect r) (cdar r))


(define (rect-area r1)
    (let ((s1 (lseg-rect r1))
          (s2 (bseg-rect r1)))
        (* (segment-length s1) (segment-length s2))))

(define (rect-peri r1)
    (let ((s1 (lseg-rect r1))
          (s2 (bseg-rect r1)))
      (* 2 (+ (segment-length s1) (segment-length s2)))))


; rect-area and rect-peri depend on lseg-rect and bseg-rect
; so any new repr of rectangle does not affect area and peri



; ---------------------------------
; Exercise 2.4
; ---------------------------------

(define (my-cons x y)
    (lambda (m) (m x y)))

(define (my-car z)
    (z (lambda (p q) p)))

(define (my-cdr z)
    (z (lambda (p q) q)))



; ---------------------------------
; Exercise 2.5
; ---------------------------------

(define (num-cons a b)
    (* (expt 2 a) (expt 3 b)))

(define (num-car z)
    (define (iter acc val)
        (if (not (= (modulo val 2) 0))
            acc
            (iter (+ acc 1) (/ val 2))))
    (iter 0 z))

(define (num-cdr z)
    (define (iter acc val)
        (if (not (= (modulo val 3) 0))
            acc
            (iter (+ acc 1) (/ val 3))))
    (iter 0 z))



; ---------------------------------
; Exercise 2.7, 2.8
; ---------------------------------

(define (make-interval a b) (cons a b))

(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))


; Interval Arithmetic
; https://en.wikipedia.org/wiki/Interval_arithmetic

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))


(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))


(define (div-interval x y)
    (if (spans-zero? y)
        (error "divisor interval spans zero")
        (mul-interval
            x
            (make-interval (/ 1.0 (upper-bound y))
                           (/ 1.0 (lower-bound y))))))



; ---------------------------------
; Exercise 2.9
; ---------------------------------

(define (width-interval x)
    (/ (- (upper-bound x) (lower-bound x)) 2))

(define (compare-width op x y)
    (cond ((equal? op "add")
            (= (width-interval (add-interval x y))
               (+ (width-interval x) (width-interval y))))
          ((equal? op "sub")
            (= (width-interval (sub-interval x y))
               (+ (width-interval x) (width-interval y))))          ; note the + here due to how interval arithmetic works
          ((equal? op "mul")
            (= (width-interval (mul-interval x y))
               (* (width-interval x) (width-interval y))))
          ((equal? op "div")
            (= (width-interval (div-interval x y))
               (/ (width-interval x) (width-interval y))))
          (else error "Unknown OP | Use add, sub, mul, div")))



; ---------------------------------
; Exercise 2.10
; ---------------------------------

; Checks if an interval contains 0
(define (spans-zero? x)
    (and (<= (lower-bound x) 0)
         (>= (upper-bound x) 0)))

; updated div-interval procedure accordingly
