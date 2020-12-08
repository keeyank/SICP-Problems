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
  
  
(define (cbrt x)
  (define (improve guess)
    (/ (+
	(/ x (square guess))
	(* 2 guess))
       3))
  (define (good-enough? guess)
    (< (abs (- x (square guess))) .00001))
  (define (cbrt-iter guess)
    (if (good-enough? guess)
	guess
	(cbrt-iter (improve guess))))
  (cbrt-iter 9))
	
