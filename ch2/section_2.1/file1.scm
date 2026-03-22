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
