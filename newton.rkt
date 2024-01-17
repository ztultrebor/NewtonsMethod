;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname newton) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define (derivative coeff-list)
  ; [ListOf N] -> [ListOf N]
  ; takes the coefficients for a polynomial and returns
  ; the coefficients of that polynomial's derivative
  (rest (map * coeff-list (build-list (length coeff-list) identity))))


(define (make-func coefficients)
  ; [ListOf N] -> [Number -> Number]
  ; takes as input a list of corfficients and returns a function
  ; that describes the polynomial with those coefficients
  (lambda (x) 
    (foldr
     (λ (c xn rem) (+ (* c xn) rem))
     #i0
     coefficients
     (build-list (length coefficients) (λ (n) (expt x n))))))


(define (newton coeffs guess)
  ; [ListOf Number] Number -> [Maybe Number]
  ; takes as an argument a list of integer coefficients which gets
  ; interpreted as the coefficients of a polynomial function, as well as
  ; an initial guess at one of the roots
  ; It then finds the nearest root to guess using Newton's Method
  (local (
          (define tol 1e-6)
          (define f (make-func coefficients))
          (define df (make-func (derivative coefficients)))
          (define (method root i)
            ; Number N -> [Maybe Number]
            ; using a recursive approach,
            ; homes in on the nearest root using Newton's Method,
            ; stopping once f(root) is sufficiently close to 0,
            ; or if it has too much trouble finding a zero
            (cond
              [(> i 32) #f]
              [else (local (
                            (define f@root (f root)))
                      (if (< (abs f@root) tol) root
                          (method (- root (/ f@root (df root))) (add1 i))))])))
    ; - IN -
    (method guess 0)))
     
            
(define (find-all-zeros coeffs)
  ; [ListOf N] -> [ListOf Number]
  ; A best attempt to return all the roots of a given polynomial
  (local (
          (define range 64)
          (define starting-points
            (build-list (+ (* 2 range) 1) (lambda (n) (/ (- n range) 2))))
          (define raw-results (map (λ (g) (newton coeffs g)) starting-points))
          (define approx-solutions
            (filter (λ (r) (not (false? r))) raw-results))
          (define solutions
            (map (λ (s) (/ (round (* 1e6 (inexact->exact s))) 1e6))
                 approx-solutions)))
    ; - IN -
    (sort (settle solutions) <)))


(define (settle lst)
  ; [ListOf X] -> [ListOf X]
  ; converts a list into a set
  (cond
    [(empty? lst) '()]
    [(member? (first lst) (rest lst)) (settle (rest lst))]
    [else (cons (first lst) (settle (rest lst)))]))



; ==========================
; action!

(define order 8)
(define coefficients (build-list 8 (λ (n) (- (random 201) 100))))
;(define coefficients (list 67 -98 2 -38 46 0 42 2)) ; hard case here
;(define coefficients (list -24 -5 -88 34 86 -59 -1 -24)) ; another one
;(define coefficients (list -27 1 84 86 -38 -94 -9 -84)) ; again
;(define coefficients (list -34 -97 94 53 16 81 -71 13)) ; ibid
coefficients
(find-all-zeros coefficients)