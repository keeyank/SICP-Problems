(Define (square x) (* x x))
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))
(define (average3 a b c) (/ (+ a b c) 3))
(define (abs a) (if (< a 0) (- a) a))
(define (sign a) (if (< a 0) -1 1))
(define (div a b) (truncate (/ a b)))
(define (inc n) (+ n 1))

(define (<= a b) (not (> a b)))
(define (>= a b) (not (< a b)))
(define (!= a b) (not (= a b)))

(define (even? n) (= (remainder n 2) 0))
(define (same-parity? a b) (= (remainder a 2)
			      (remainder b 2)))

;;5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
		 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
		 trials-passed))))
  (iter trials 0))

(define (random-range low high)
  (let ((range (- high low)))
    (+ (random range) low)))
(random-range 5 8)

(define (estimate-integral p x1 x2 y1 y2)
  (define (experiment)
    (let ((x (random-range x1 (+ .0 x2)))
	  (y (random-range y1 (+ .0 y2))))
      (p x y)))
  
  (let ((factor (monte-carlo 10000 experiment))
	(rect-area (* (- x2 x1)
		      (- y2 y1))))
    (* (+ factor .0) rect-area)))

(define pi
  (let ((in-unit-circle?
	 (lambda (x y) (<= (+ (square x)
			      (square y))
			   1))))
    (estimate-integral in-unit-circle?  -1 1 -1 1)))
      
;; Note how modular the monte carlo method is, all it requires is an "experiment" procedure. In our case, we base our experiment on the parameters provided to estimate-integral.
;; We add .0 to x2 and y2 to ensure they are decimals

;;6
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define (rand x)
  (define (generate)
    (set! x (rand-update x))
    x)
  (define (reset new-val)
    (set! x new-val)
    x)
  
  (define (dispatch m)
    (cond ((eq? m 'generate)
	   generate)
	  ((eq? m 'reset)
	   reset)))
  dispatch)

(define seed (rand 3)) ;; 3 is the initial seed
((seed 'reset) 2)
((seed 'generate))

(define seed2 (rand 3))
((seed2 'generate))
((seed2 'reset) 4)

;; Because seed is set to the dispatch procedure returned by rand, we retain it's environment and it's state is updated as we utilize the dispatch procedure (aka seed) to generate new random numbers, or reset the number.
