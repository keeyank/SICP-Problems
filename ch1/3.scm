(define (<= a b) (or (< a b) (= a b)))

(define (square a) (* a a))

(define (sum-squares a b)
  (+ (square a) (square b)))

(define (sum-larger-squares a b c)
  (cond ((and (<= c a) (<= c b))
	 (sum-squares a b))
	((and (<= b a) (<= b c))
	 (sum-squares a c))
	((and (<= a b) (<= a c))
	 (sum-squares b c))))
   
