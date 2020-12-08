(define (square x) (* x x))
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
(define (for-each proc items)
  (cond ((null? items)
	 true)
	(else
	 (proc (car items))
	 (for-each proc (cdr items)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter
	       (beside smaller smaller)))))
;;Notice how, using let (which is an implicitly called lambda procedure with parameter smaller set to the up-split call), we only call up-split once. The process is thus recursive, not iterative, but not tree either recursive (its big theta of n).

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter smaller))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside quarter (flip-horiz quarter))))
      (below (flip-vert half) half))))


(define (split paint-proc1 paint-proc2)
  (define (split1 painter n)
    (if (= n 0)
	painter
	(let ((smaller (split1 painter (- n 1))))
	  (paint-proc1 painter
		       (paint-proc2 smaller smaller)))))
  split1)

(define up-split (split below beside))

(define (make-vec x y) (cons x y))
(define (xcor-vec v) (car v))
(define (ycor-vec v) (cdr v))

(define (add-vec v1 v2)
  (make-vec (+ (xcor-vec v1) (xcor-vec v2))
	    (+ (ycor-vec v1) (ycor-vec v2))))
(define (sub-vec v1 v2)
  (make-vec (- (xcor-vec v1) (xcor-vec v2))
	    (- (ycor-vec v1) (ycor-vec v2))))
(define (scale-vec c v)
  (make-vec (* c (xcor-vec v))
	    (* c (ycor-vec v))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))

(define myorigin (make-vec 2 2))
(define myedge1 (make-vec 18 0))
(define myedge2 (make-vec 0 18))

(define myframe (make-frame myorigin myedge1 myedge2))
(edge1-frame myframe)
(edge2-frame myframe)
(origin-frame myframe)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vec
     (origin-frame frame)
     (add-vec (scale-vec (xcor-vec v)
                           (edge1-frame frame))
               (scale-vec (ycor-vec v)
                           (edge2-frame frame))))))

(define (make-segment v1 v2) (cons v1 v2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define v1 (make-vec 0 0))
(define v2 (make-vec 0 1))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define outline (list (make-segment (make-vec 0. 0.)
				    (make-vec 1. 0.))
		      (make-segment (make-vec 1. 0.)
				    (make-vec 1. 1.))
		      (make-segment (make-vec 1. 1.)
				    (make-vec 0. 1.))
		      (make-segment (make-vec 0. 1.)
				    (make-vec 0. 0.))))
(define painter-outline (segments->painter outline))
(painter-outline myframe)

(define x (list (make-segment (make-vec 0. 0.)
			      (make-vec 1. 1.))
		(make-segment (make-vec 1. 0.)
			      (make-vec 0. 1.))))
(define painter-x (segments->painter x))
(painter-x myframe)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vec (m corner1) new-origin)
                     (sub-vec (m corner2) new-origin)))))))

;;50
;; flip-vert returns a procedure that accepts a frame
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vec 1. 0.)
		     (make-vec 0. 0.)
		     (make-vec 0. 1.)))
;; Use example
((flip-horiz painter-x) myframe)

