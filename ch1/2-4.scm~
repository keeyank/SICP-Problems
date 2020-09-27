(define (square x) (* x x))
(define (cube x) (* x x x))
(define (average a b) (/ (+ a b) 2))
(define (average3 a b c) (/ (+ a b c) 3))
(define (abs a) (if (< a 0) (- a) a))
(define (div a b) (truncate (/ a b)))
(define (inc n) (+ n 1))

(define (<= a b) (not (> a b)))
(define (>= a b) (not (< a b)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((= d 0)
	   (error "0 cannot be denominator"))
	  ((or (and (< n 0) (< d 0))
	       (and (>= n 0) (< d 0)))
	   (cons (- (/ n g)) (- (/ d g))))
	  (else
	   (cons (/ n g) (/ d g))))))

(define (numer r) (car r))
(define (denom r) (cdr r))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat r)
  (newline)
  (display (numer r))
  (display "/")
  (display (denom r)))
  
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2)) ;Pass-by-val or ref??
					;Does it even make a difference? There doesn't seem to be a way to modify data in Scheme (atleast we haven't encountered any yet...)

(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
					;Note: You can actually do (make-segment 1 2) instead of with the actual points, since there's no type-checking.

(define (midpoint s)
  (make-point (average (x-point (start-segment s))
		       (x-point (end-segment s)))
	      (average (y-point (start-segment s))
		       (y-point (end-segment s)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(define (make-rect bl tr)
  (if (or (> (car bl) (car tr)) (> (cdr bl) (cdr tr)))
      (error "Bad specification")
      (cons (make-point (car bl) (cdr bl))
	    (make-point (car tr) (cdr tr)))))
					;This method will make an error pop up if they pass in a non-pair. However, weird behaviour still if they have a pair that has another pair as the car or cdr!
(define (bl r) (car r))
(define (tr r) (cdr r))

(define (width r)
  (- (x-point (tr r))
     (x-point (bl r))))
(define (height r)
  (- (y-point (tr r))
     (y-point (bl r))))

(define (perimeter r) ; BAD
  (* 2 (+ (- (x-point (tr r))
	     (x-point (bl r)))
	  (- (y-point (tr r))
	     (y-point (bl r))))))

(define (perimeter r) ; GOOD
  (+ (* 2 (width r))
     (* 2 (height r))))

(define (area r) ; BAD
  (* (- (x-point (tr r))
	(x-point (bl r)))
     (- (y-point (tr r))
	(y-point (bl r)))))

(define (area r) ; GOOD
  (* (width r) (height r)))

; Once we added the abstraction layer width and height, it made area and perimeter much easier to define. Always use abstraction layers when they can be used to simplify procedures in higher layers - it helps you understand the programs you write better, and can help modify programs much more easily later down the line.

(define (make-rect bl br tr tl)
  (cons (cons bl br) (cons tr tl)))

(define (bl r) (car (car r)))
(define (br r) (cdr (car r)))
(define (tr r) (car (cdr r)))
(define (tl r) (cdr (cdr r)))

(define (width r)
  (- (x-point (br r))
     (x-point (bl r))))
(define (height r)
  (- (y-point (tl r))
     (y-point (bl r))))
	      
(define rect3 (make-rect (make-point 1 1)
			 (make-point 4 1)
			 (make-point 4 4)
			 (make-point 1 4)))
(perimeter rect3) ;12
(area rect3) ;9

; We modified the internal representation of the compound data, but notice how with the better definitions of perimeter and area, we didn' have to modify them at all!!! We just needed to modify our width and height abstraction functions.
