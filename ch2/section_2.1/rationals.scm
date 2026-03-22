(define (negative? x) (< x 0))


; ---------------------------------
; Rational Number Constructor/Selectors
; ---------------------------------

(define (make-rat n d)
    (let ((g (gcd n d)))
        (if (negative? d)
            (cons (/ (- n) g) (/ (- d) g))
            (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))



; ---------------------------------
; Rational Number Arithmetic
; ---------------------------------

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))



; Print rational number
(define (print-rat x)
    ; (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))
