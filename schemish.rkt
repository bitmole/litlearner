#lang racket
(require malt)

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

(define line
  (lambda (x)
    (lambda (theta)
      (+ (* (ref theta 0) x) (ref theta 1)))))

(define nestcnt
  (lambda (t)
    (cond
      ((scalar? t) 0)
      (else (add1 (nestcnt (tref t 0)))))))
