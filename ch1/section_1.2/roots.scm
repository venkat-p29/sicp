; #lang sicp


(define threshold 0.001)

; SQUARE ROOT

(define (square x) (* x x))

(define (my-sqrt x)
    (define (good-enough? guess)
      (< (abs (- (square guess) x)) threshold))
    (define (improve guess)
      (/ (+ guess (/ x guess)) 2))
    (define (sqrt-iter guess)
      (if (good-enough? guess)
          guess
          (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))



; CUBE ROOT

(define (cube x) (* x x x))

(define (my-cbrt x)
    (define (good-enough-cube? guess)
      (< (abs (- (cube guess) x)) threshold))
    (define (improve-cube-guess guess)
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
    (define (cbrt-iter guess)
      (if (good-enough-cube? guess)
          guess
          (cbrt-iter (improve-cube-guess guess))))
  (cbrt-iter 1.0))
