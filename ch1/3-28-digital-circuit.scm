
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

;; List processing

(define (equals? l1 l2)
  (cond ((and (pair? l1) (pair? l2))
	 (and (equal? (car l1) (car l2))
	      (equal? (cdr l1) (cdr l2))))
	((not (or (pair? l1) (pair? l2)))
	 (eq? l1 l2))
	(else false)))

(define (map proc l)
  (if (null? l)
      l
      (cons (proc (car l))
	    (map proc (cdr l)))))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
	    (append (cdr l1) l2))))

(define (accumulate op init l)
  (if (null? l)
      init
      (op (car l) (accumulate op init (cdr l)))))

(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

;;;;;;

;; Digital Circuit code
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and x y)
  (cond ((and (= x 1) (= y 1)) 1)
	((or (= x 0) (= y 0)) 0)
	(else (error "Invalid signal" x y))))

;;28
(define (or-gate input1 input2 output)
  (define (or-action-procedure)
    (let (new-val (logical-or (get-signal input1)
			      (get-signal input2)))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-val)))))
  (add-action! input1 or-action-procedure)
  (add-action! input2 or-action-procedure)
  'ok)

(define (logical-or x y)
  (cond ((or (= x 1) (= y 1)) 1)
	((and (= x 0) (= y 0)) 0)
	(else (error "Invalid signal" x y))))

;;29

(define (or-gate input1 input2 output)
  (let ((w1 (make-wire))
	(w2 (make-wire))
	(w3 (make-wire)))
    (inverter input1 w1)
    (inverter input2 w2)
    (and-gate w1 w2 w3)
    (inverter w3 output)
    'ok))

;;30
(define (ripple-carry-adder a-list b-list s-list c-wire)
  (define (rca-rec al bl sl c-input)
    (let ((c-curr (make-wire)))
      (if (null? (cdr al))
	  (full-adder (car al) (car bl) c-input (car sl) c-wire)
	  (begin
	    (full-adder (car al) (car bl) c-input (car sl) c-curr)
	    (rca-rec (cdr al) (cdr bl) (cdr sl) c-curr)))))
	  
  (let ((c-init (make-wire)))
    (set-signal! c-init 0)
    (if (and (= (length a-list) (length b-list))
	     (= (length b-list) (length s-list)))
	(rca-rec a-list b-list s-list c-init)
	(error "Lists must have the same length"))))
