(define (square n) (* n n))
(define (even? n) (= (remainder n 2) 0))

(define (mult a b)
  (cond ((= b 0) 0)
	((even? b) (mult (+ a a) (/ b 2)))
	(else (+ a (mult a (- b 1))))))

(define (fast-exp b n)
  (define (fast-exp-itr b n a)
    (cond ((= n 1) (* a b))
	  ((even? n) (fast-exp-itr (square b) (/ n 2) a))
	  (else (fast-exp-itr b (- n 1) (* a b)))))
  (fast-exp-itr b n 1))
