#lang sicp

(define (square x) (* x x))

; Arithmethic operations on complex numbers


; SELECTORS

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
    (sqrt (+ (square (car z))
             (square (cdr z)))))

(define (angle z)
    (atan (cdr z) (car z)))


; CONSTRUCTORS

(define (make-rectangular x y) 
    (cons x y))

(define (make-polar r a)
    (cons (* r (cos a)) (* r (sin a))))


; ARITHMETIC

(define (+c z1 z2)
    (make-rectangular
        (+ (real-part z1) (real-part z2))
        (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
    (make-rectangular
        (- (real-part z1) (real-part z2))
        (- (imag-part z1) (imag-part z2))))

(define (* z1 z2)
    (make-polar
      (* (magnitude z1) (magnitude z2))
      (+ (angle z1) (angle z2))))

(define (/ z1 z2)
    (make-polar
      (/ (magnitude z1) (magnitude z2))
      (- (angle z1) (angle z2))))
