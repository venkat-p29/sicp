; ---------------------------------
; Covers exercises 1.35 to 1.46
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



; ---------------------------------
; Newton's Method
; ---------------------------------

(define (square x) (* x x))

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))


; Newton's Method
(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))


; General fixed-point procedure
(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess))


; so now sqrt can be expressed as
(define (sqrt-avg-damp x)
    (fixed-point-of-transform
        (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt-newtons x)
    (fixed-point-of-transform
        (lambda (y) (- (square y) x)) newton-transform 1.0))



; ---------------------------------
; Exercise 1.40
; ---------------------------------

(define (cube x) (* x x x))

(define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (cubic-roots-f a b c)
    (newtons-method (cubic a b c) 1))



; ---------------------------------
; Exercise 1.41
; ---------------------------------

(define (inc x) (+ x 1))

(define (double f)
    (lambda (x) (f (f x))))



; ---------------------------------
; Exercise 1.42
; ---------------------------------

(define (compose f g)
    (lambda (x) (f (g x))))



; ---------------------------------
; Exercise 1.43
; ---------------------------------

(define (repeated f n)
    (define (apply g i)
        (if (= i 1)
            g
            (apply (compose f g) (- i 1))))
    (apply f n))


; recursive version
(define (repeated-recur f n)
    (if (= n 1)
        f
        (compose f (repeated-recur f (- n 1)))))



; ---------------------------------
; Exercise 1.44
; ---------------------------------

(define (smooth f)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smoothed f n)
    ((repeated smooth n) f))



; ---------------------------------
; Exercise 1.45
; ---------------------------------

(define (nth-root x n)
    (define k (floor (log n 2)))                            ; got this from AI
    (define nth-f (lambda (y) (/ x (expt y (- n 1)))))
    (fixed-point
        ((repeated average-damp k) nth-f)
        1.0))



; ---------------------------------
; Exercise 1.46
; ---------------------------------

(define (iterative-improve good-enough? improve-guess)
    (lambda (guess)
        (let ((next (improve-guess guess)))
            (if (good-enough? guess next)
                next
                ((iterative-improve good-enough? improve-guess) next)))))

(define (fixed-point-iter f)
    (define tolerance 0.00001)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
        tolerance))
    (iterative-improve close-enough? f))
