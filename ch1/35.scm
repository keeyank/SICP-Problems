(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))
(define (abs a) (if (< a 0) (- a) a))
(define (div a b) (truncate (/ a b)))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)

(define (cont-frac n d k)
  (define (cont-frac-rec i)
    (if (> i k)
	0
	(/ (n i) (+ (d i) (cont-frac-rec (+ i 1))))))
  (cont-frac-rec 1))

(define (cont-frac n d k)
  (define (cont-frac-itr i accum)
    (if (< i 1)
	accum
	(cont-frac-itr (- i 1) (/ (n i) (+ (d i) accum)))))
  (cont-frac-itr k 0))

(define golden-ratio (/ 1 (cont-frac (lambda (i) 1.)
				     (lambda (i) 1.)
				     100)))
(define (euler-pattern i)
  (if (= (remainder i 3) 2)
      (* 2. (+ 1 (div i 3)))
      1.))

(define e (+ 2 (cont-frac (lambda (x) 1.) euler-pattern 100)))

;; 39
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
	x
	(- (square x))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac n d k))
    
