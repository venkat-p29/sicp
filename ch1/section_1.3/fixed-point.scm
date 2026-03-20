; ---------------------------------
; Covers exercises 1.35 to
; ---------------------------------


; ---------------------------------
; Fixed point function
; ---------------------------------

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
        tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))


(define (average a b)
    (/ (+ a b) 2))



; ---------------------------------
; Exercise 1.35
; ---------------------------------

(define golden-ratio
    (fixed-point (lambda (x) (+ 1 (/ x))) 1.0))



; ---------------------------------
; Exercise 1.36
; ---------------------------------

(define (pretty-print val)
    (newline)
    (display val))

(define (print-fp f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
        tolerance))
    (define (try guess)
        (pretty-print guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                (pretty-print next)
                (try next))))
    (try first-guess))


(define pow-f
    (lambda (x) (/ (log 1000) (log x))))

(define (x-pow-x)
    (print-fp pow-f 2.0))

(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (damped-x-pow-x)
    (print-fp (average-damp pow-f) 2.0))



; ---------------------------------
; Exercise 1.37
; ---------------------------------

(define (cont-frac n d k)
    (define (recur n d i)
        (if (> i k)
            0
            (/ (n i) (+ (d i) (recur n d (+ i 1))))))
    (recur n d 1))


(define (iter-cont-frac n d k)
    (define (iter i result)
        (if (< i 1)
            result
            (iter (- i 1) (/ (n i) (+ (d i) result)))))
    (iter k 0))
