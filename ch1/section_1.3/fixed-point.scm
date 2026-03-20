; ---------------------------------
; Covers exercises 1.35 to 1.39
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



; ---------------------------------
; Exercise 1.38
; ---------------------------------

(define (e-n i) 1.0)

(define (e-d i) 
    (define (is-series? x)
        (= 0 (modulo (- x 5) 3)))
    (cond ((= i 1) 1)
          ((= i 2) 2)
          ((is-series? i) (* (+ (/ (- i 5) 3) 2) 2))
          (else 1)))

(define (euler-e k)
    (+ 2 (cont-frac e-n e-d k)))



; ---------------------------------
; Exercise 1.39
; ---------------------------------

(define (tan-cf x k)
    (define (n-tan i)
        (if (= i 1)
            x
            (* x x)))

    (define (d-tan i)
        (- (* 2 i) 1))


    ; Changing + to - for tan function
    (define (iter-cont-f n d k)
        (define (iter i result)
            (if (< i 1)
                result
                (iter (- i 1) (/ (n i) (- (d i) result)))))
        (iter k 0))

    (iter-cont-f n-tan d-tan k))
