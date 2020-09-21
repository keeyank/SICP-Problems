(define (square x) (* x x))
(define (abs x)
  (if (< x 0) (- x) x))
(define (avg a b)
  (/ (+ a b) 2))

(define (sqrt x)

  (define (fraction-diff guess prev-guess)
    (/ (abs (- guess prev-guess)) guess))
  
  (define (good-enough? guess prev-guess)
    (< (fraction-diff guess prev-guess) 0.000001))
  
  (define (improve guess)
    (avg guess (/ x guess)))
  
  (define (sqrt-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
	guess
	(sqrt-iter (improve guess) guess)))
  
  (sqrt-iter 1.0 2.0))
  
  
