#lang racket
(require malt)

; warmup
(define pie 3.14)
(define a-radius 8.4)

(define an-area
  (* pie 
     (* a-radius a-radius)))

(define area-of-circle
  (lambda (r)
    (* pie
       (* r r))))

(define area-of-rectangle
  (lambda (width)
    (lambda (height)
      (* width height))))

(define double-result-of-f
  (lambda (f) 
    (lambda (z)
      (* 2 (f z)))))

(define abs
  (lambda (x)
    (cond
      ((< x 0) (- 0 x))
      (else x))))

; warmup recursion
(define remainder
  (lambda (x y)
    (cond
      ((< x y) x)
      (else (remainder (- x y) y)))))

(define add
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (add n (sub1 m)))))))

; on to serious business:
; give us a line function in which we need to customize weight and bias 
(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

; rank of tensor
(define rank
  (lambda (t)
    (ranked t 0)))

(define ranked
  (lambda (t a)
    (cond
      ((scalar? t) a)
      (else (ranked (tref t 0) (add1 a))))))

; shape of tensor
(define shape
  (lambda (t)
    (cond 
      ((scalar? t) '())
      (else (cons (tlen t) (shape (tref t 0)))))))

; product of list members
(define product
  (lambda (l)
    (cond
      ((empty? l) 1)
      (else (* (car l) (product (cdr l)))))))

; we can use tensor shape to get the number of scalars
(define nscalars
  (lambda (t)
    (product (shape t))))
