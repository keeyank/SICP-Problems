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

;;; ALL procedures in 2.5.3 except make-polynomial
;;; should be inserted in install-polynomial-package, as indicated
;; Thus, we can get rid of the "poly" postfix on most procedure names.
;; However, we need it for most actually - Consider the fact that we
;; use the generic add procedure, and the specific add-poly procedure
;; throughout the package. So we must distinguish them.

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

;; 88
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
		 (add-terms (term-list p1)
			    (term-list (negate-poly p2))))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))
;; Alternate: We could have just negate terms on (term-list p2)

(define (negate-poly p) ;; Good idea to add this to the generic operations too
  (make-poly (variable p) 
	     (negate-terms (terms p))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

;;87
(define (equal-zero? p)
  (cond ((empty-termlist? p)
	 true)
	((not (= (coeff (first-term p)) 0))
	 false)
	(else
	 (equal-zero? (rest-terms p)))))

(define (=zero? object)
  (apply-generic '=zero? object))
     
  

;; *incomplete* skeleton of package
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;;[procedures same-variable? and variable? from section 2.3.2]

  ;; representation of terms and term lists
  ;;[procedures adjoin-term ... coeff from text below]

  ;;(define (add-poly p1 p2) ... )
  ;;[procedures used by add-poly]

  ;;(define (mul-poly p1 p2) ... )
  ;;[procedures used by mul-poly]

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'neg 'polynomial
       (lambda (p) (tag (negate-poly p))))
  (put '=zero? 'polynomial equal-zero?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

;; 88
(define (negate-terms l)
  (if (empty-termlist? l)
      l
      (cons (make-term (order (first-term l))
		       (negate (coeff (first-term l)))
	    (negate-terms (rest-terms l))))))
;; Notice how we use the generic negate here, so this works when the coefficients are polynomials

;; Representing term lists

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

;; 89
(define (first-term term-list)
  (make-term (- (length term-list) 1)
	     (car term-list)))

;; We don't need to use generic operations for comparison of orders - we know these will be regular scheme-numbers
;; Assumption: This won't be called with an order that has an existing non-zero coefficient.
(define (adjoin-term term term-list)
  (let ((first (first-term term-list)))
    (cond ((> (order term) (order first))
	   (adjoin-term term
			(cons (make-term (+ 1 (order first))
					 0)
			      term-list)))
	  ((= (order term) (order first))
	   (cons (coeff term) (rest-terms term-list)))
	  ((< (order term) (order first))
	   (cons (coeff first)
		 (adjoin-term term (rest-terms term-list)))))))

;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; 90
;; We would also create a separate sparse-terms-package installation procedure
(define (install-dense-terms-package)
  (define (adjoin-term term term-list)
    (let ((first (first-term term-list)))
      (cond ((> (order term) (order first))
	     (adjoin-term term
			  (cons (make-term (+ 1 (order first))
					   0)
				term-list)))
	    ((= (order term) (order first))
	     (cons (coeff term) (rest-terms term-list)))
	    ((< (order term) (order first))
	     (cons (coeff first)
		   (adjoin-term term (rest-terms term-list)))))))

  (define (the-empty-termlist) '())
  (define (make-term order coeff)
    (list order coeff))
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
	       (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (tag term-list) (attach-tag 'dense term-list))
  
  (put 'adjoin-term '(term dense)
       (lambda (term term-list)
	 (tag (adjoin-term term term-list))))
  (put 'first-term 'dense
       (lambda (term-list)
	 (tag (first-term term-list))))
  'done)

;; Need to attach term tag to ensure apply-generic dispatches to the correct procedure when calling adjoin-term.
(define (make-term order coeff)
  (attach-tag 'term (list order coeff)))
;; We also need to attach 'dense tag to empty termlist procedure. Then, subsequent calles to adjoin-term will dispatch on the appropriate procedure.
(define (the-empty-termlist)
  (attach-tag 'dense '()))

(define (adjoin-term term term-list)
  (apply-generic 'adjoin-term term term-list))
(define (first-term term-list)
  (apply-generic 'first-term term-list))
;; Rest of procedures here, don't need to apply generic though since they are shared between the two implementations

(define (rest-terms term-list) (cdr term-list))
(define ...)

;; Notice there is some redundancy here
