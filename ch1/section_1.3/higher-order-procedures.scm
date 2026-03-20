; ---------------------------------
; Covers exercises 1.29 to 1.33
; ---------------------------------


(load "../section_1.2/prime-num-test.scm")


(define (sum-integers a b)
    (if (> a b)
        0
        (+ a (sum-integers (+ a 1) b))))


; A generic function for summation

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))


(define (cube x) (* x x x))
(define (inc n) (+ n 1))

(define (sum-cubes a b)
    (sum cube a inc b))


; Integral Sum

(define (integral f a b dx)
    (define (add-dx x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
    dx))


; ---------------------------------
; Exercise 1.29 - Simpson's Rule
; ---------------------------------

(define (integral-f f a b n)
    (define h (/ (- b a) n))
    (define (next-y k) (f (+ a (* k h))))
    (define (coeff k)
        (cond ((or (= k 0) (= k n)) 1)
              ((even? k) 2)
              (else 4)))
    (define (add-f acc k)
        (if (> k n)
            acc
            (add-f (+ acc (* (coeff k) (next-y k))) (+ k 1))))
    (* (/ h 3) (add-f 0 0)))


; ---------------------------------
; Exercise 1.30 - Iter sum
; ---------------------------------

(define (sum-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))


; ---------------------------------
; Exercise 1.31 - Product
; ---------------------------------

(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))


(define (prod-iter term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))


; Factorial

(define (identity x) x)
(define (fact n)
    (product identity 1 inc n))
(define (fact-iter n)
    (prod-iter identity 1 inc n))


; Approx to pi

(define (pi-term i)
    (if (odd? i)
        (/ (+ i 1) (+ i 2))
        (/ (+ i 2) (+ i 1))))
(define (close-to-pi n)
    (* 4.0 (prod-iter pi-term 1 inc n)))


; ---------------------------------
; Exercise 1.32 - accumulate
; ---------------------------------

(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))))


(define (acc-sum term a next b)
    (accumulate + 0 term a next b))


(define (acc-product term a next b)
    (accumulate * 1 term a next b))


; Iter version

(define (accumulate-iter combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))


(define (acc-sum-iter term a next b)
    (accumulate-iter + 0 term a next b))


(define (acc-product-iter term a next b)
    (accumulate-iter * 1 term a next b))


; ---------------------------------
; Exercise 1.33 - filtered-accumulate
; ---------------------------------

(define (filtered-accumulate filter-by combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (if (filter-by a)
                (iter (next a) (combiner result (term a)))
                (iter (next a) result))))
    (iter a null-value))


(define (sum-sq-prime a b)
    (filtered-accumulate prime? + 0 square a inc b))


(define (relative-prime? y)
    (lambda (x) (= (gcd y x) 1)))


(define (prod-relative-prime n)
    (filtered-accumulate (relative-prime? n) * 1 identity 1 inc n))
