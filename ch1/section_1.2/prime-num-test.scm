; #####################################################################

; Checking if a number is prime by using sqrt method


(define (square x) (* x x))

(define (next n)
    (if (= n 2) 
        3
        (+ n 2)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
    (= n (smallest-divisor n)))


;#######################################################################

; Fermat’s Little Theorem: 
; If n is a prime number and a is any positive integer less than n, 
; then a raised to the nth power is congruent to a modulo n.


; Calculate power(a, n) using odd-even square method and check if power(a, n) mod n == a
(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
              (remainder
                (square (expmod base (/ exp 2) m))
                m))
         (else
             (remainder
                (* base (expmod base (- exp 1) m))
                m))))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))


;#######################################################################

; A function to track the time it took to run the primality test

(define (runtime)
  (current-inexact-milliseconds))

(define (timed-prime-test n)
    (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime n (- (runtime) start-time))
        ; (display " not a prime")))
        #f))
(define (report-prime n elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))


;#######################################################################

(define (search-for-primes low high)
    (timed-prime-test (+ low 1))
    (if (< (+ low 1) high)
        (search-for-primes (+ low 2) high)
        (display "\nDone")))
