
; Exercise 1.11

(define (f n)
  (cond ((< n 3) n)
        (else (+ 
                (f (- n 1))
                (* 2 (f (- n 2)))
                (* 3 (f (- n 3)))
        ))))


(define (iter-f n)
  (if (< n 3)
      n
      (iter 0 1 2 (- n 2))))
(define (iter a b c remaining)
  (if (= remaining 0)
      c
      (iter b
            c
            (+ c (* 2 b) (* 3 a))
            (- remaining 1))))



; Exercise 1.12
; Rows and Cols begin from 1

(define (pascal row col)
  (cond ((> (- col row) 1) error "Invalid (row, col) values")
        ((or (= col 1) (= col (+ row 1))) 1)
        (else (+ 
                (pascal (- row 1) (- col 1))
                (pascal (- row 1) col)))))

