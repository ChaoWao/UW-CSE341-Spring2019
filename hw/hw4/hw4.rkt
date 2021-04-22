
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
; Problem 1
(define (sequence spacing low high)
  (if (> low high)
      null
      (cons low (sequence spacing (+ low spacing) high))))

; Problem 2
(define (string-append-map xs suffix)
  (map
   (lambda (a) (string-append a suffix))
   xs))

; Problem 3
(define (list-nth-mod  xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (equal? xs null)
          (error "list-nth-mod: empty list")
          (let* ([len (length xs)]
                 [rem (remainder n len)])
            (car (list-tail xs rem))))))

; Problem 4
(define (stream-for-k-steps s k)
  (if (= k 0)
      null
      (cons (car (s)) (stream-for-k-steps (cdr (s)) (- k 1)))))

; Problem 5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons
                           (if (= 0 (remainder x 6))
                               (- 0 x)
                               x)
                           (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

; Problem 7
(define (stream-add-one s)
  (lambda () (cons
              (cons 1 (car (s)))
              (stream-add-one (cdr (s))))))

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons
                           (cons (list-nth-mod xs n) (list-nth-mod ys n))
                           (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

; Problem 9
(define (vector-assoc v vec)
  (letrec ([helper (lambda (i)
                     (if (< i (vector-length vec))
                         (let ([vi (vector-ref vec i)])
                           (if (pair? vi)
                               (if (equal? v (car vi))
                                   vi
                                   (helper (+ i 1)))
                               (helper (+ i 1))))
                         #f))])
    (helper 0)))

; Problem 10
(define (caching-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]
           [f (lambda (x)
                (let ([ans (vector-assoc x cache)])
                  (if ans
                      ans
                      (let ([new-ans (assoc x xs)])
                        (begin
                          (vector-set! cache pos new-ans)
                          (set! pos (remainder (+ 1 pos) n))
                          new-ans)))))])
    f))

; Problem 11
(define-syntax while-greater
  (syntax-rules (do)
    [(while-greater e1 do e2)
     (let ([r1 e1])
       (letrec ([loop (lambda ()
                        (if (< r1 e2)
                            (loop)
                            #t))])
         (loop)))]))


; Challenge Problem 12-13, not done