
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

;; Queue interface
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;;21
(define (print-queue q)
  (display (front-ptr q))
  'done)

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)

(print-queue q1)
(front-queue q1)



;;22
(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    
    (define (empty-queue?) (null? front-ptr))
    
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with an empty queue")
	  (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch)))) 

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set! front-ptr (cdr front-ptr))
             dispatch)))

    (define (print-queue)
      (display front-ptr)
      'done)

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
	    ((eq? m 'front-queue) front-queue)
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) delete-queue!)
	    ((eq? m 'print-queue) print-queue)
	    (else (error "DISPATCH called with invalid command"))))
    dispatch))

(define (empty-queue? queue) ((queue 'empty-queue)))
(define (front-queue queue) ((queue 'front-queue)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) ((queue 'delete-queue!)))
(define (print-queue queue) ((queue 'print-queue)))
;; Note the need for extra parenthesis to call the procedures obtained by the dispatch.

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)

(print-queue q1)
(front-queue q1)



;;23
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (empty-queue? deque)
  (and (null? (front-ptr deque))
       (null? (rear-ptr deque))))
(define (make-queue) (cons '() '()))

(define (front-queue deque)
  (car (front-ptr deque)))

(define (rear-deque deque)
  (car (rear-ptr deque)))

(define (rear-insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (front-insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
	  (else
	   (set-cdr! new-pair (front-ptr queue))
	   (set-front-ptr! queue new-pair)
	   queue))))

(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
	((null? (cdr (front-ptr queue))) ; 1 element in deque
	 (set-front-ptr! queue '())
	 (set-rear-ptr! queue '())
	 queue)
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
	 (error "DELETE! Called with an empty queue" queue))
	((null? (cdr (front-ptr queue)))
	 (set-front-ptr! queue '())
	 (set-rear-ptr! queue '())
	 queue)
	(else

	 ;; Idea - Have each element be a list of 3 items, a pointer to previous element, pointer to next element, and the item of the current element. This will make rear-delete-queue! a lot easier to implement, and it will be O(1).

(define (print-queue q)
  (display (front-ptr q))
  'done)

(define q1 (make-queue))
(rear-insert-queue! q1 'a)
(rear-insert-queue! q1 'b)
(front-insert-queue! q1 'c)
(front-insert-queue! q1 'd)
(front-delete-queue! q1)

(print-queue q1)
(front-queue q1)
