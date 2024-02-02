#lang racket
(require malt)

; warmup
(define pie 3.14)
(define a-radius 8.4)

(define an-area
  (* pie 
     (* a-radius a-radius)))

(define area-of-circle
  (λ (r)
    (* pie
       (* r r))))

(define area-of-rectangle
  (λ (width)
    (λ (height)
      (* width height))))

(define double-result-of-f
  (λ (f) 
    (λ (z)
      (* 2 (f z)))))

(define abs
  (λ (x)
    (cond
      ((< x 0) (- 0 x))
      (else x))))

; warmup recursion
(define remainder
  (λ (x y)
    (cond
      ((< x y) x)
      (else (remainder (- x y) y)))))

(define add
  (λ (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

; on to serious business:
; give us a line function in which we need to customize weight and bias 
(define line
  (λ (x)
    (λ (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

; rank of tensor
(define rank
  (λ (t)
    (ranked t 0)))

(define ranked
  (λ (t a)
    (cond
      ((scalar? t) a)
      (else (ranked (tref t 0) (add1 a))))))

; shape of tensor
(define shape
  (λ (t)
    (cond 
      ((scalar? t) '())
      (else (cons (tlen t) (shape (tref t 0)))))))

; product of list members
(define product
  (λ (l)
    (cond
      ((empty? l) 1)
      (else (* (car l) (product (cdr l)))))))

; we can use tensor shape to get the number of scalars
(define nscalars
  (λ (t)
    (product (shape t))))

; let's sum up a tensor-1
(define sum-1
  (λ (t)
    (summed t (sub1 (tlen t)) 0.0)))

(define summed
  (λ (t i a)
    (cond
      ((zero? i) (+ a (tref t 0)))
      (else (summed t (sub1 i) (+ a (tref t i)))))))

; generalized loss function
(define l2-loss
  (λ (target)
     (λ (xs ys)
	(λ (ϴ)
	   (let ((pred-ys ((target xs) ϴ)))
	     (sum
	       (sqr
		 (- ys pred-ys))))))))
