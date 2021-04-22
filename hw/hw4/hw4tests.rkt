#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define test11 (equal? (sequence 1 0 5) '(0 1 2 3 4 5)))
(begin (display "test11 = ") (displayln test11))
(define test12 (equal? (sequence 2 3 11) '(3 5 7 9 11)))
(begin (display "test12 = ") (displayln test12))
(define test13 (equal? (sequence 3 3 8) '(3 6)))
(begin (display "test13 = ") (displayln test13))
(define test14 (equal? (sequence 1 3 2) null))
(begin (display "test14 = ") (displayln test14))

(define test2 (equal?
               (list "dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg")
               (string-append-map 
                (list "dan" "dog" "curry" "dog2") 
                ".jpg")))
(begin (display "test2 = ") (displayln test2))

(define test3 (= 2 (list-nth-mod '(1 2 3 4 5) 6)))
(begin (display "test3 = ") (displayln test3))
;(list-nth-mod '(1 2 3 4 5) -1)
;(list-nth-mod '() 1)

(define ones (lambda () (cons 1 ones)))
(define test4 (equal? (stream-for-k-steps ones 3)
                      '(1 1 1)))
(begin (display "test4 = ") (displayln test4))

(define test5 (equal? (stream-for-k-steps funny-number-stream 16)
                      '(1 2 3 4 5 -6 7 8 9 10 11 -12 13 14 15 16)))
(begin (display "test5 = ") (displayln test5))

(define test6 (equal? (stream-for-k-steps dan-then-dog 5)
                      '("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg")))
(begin (display "test6 = ") (displayln test6))

(define test7 (equal? (stream-for-k-steps (stream-add-one dan-then-dog) 5)
                      '((1 . "dan.jpg") (1 . "dog.jpg") (1 . "dan.jpg") (1 . "dog.jpg") (1 . "dan.jpg"))))
(begin (display "test7 = ") (displayln test7))

(define test8 (equal?
               (stream-for-k-steps (cycle-lists '(1 2 3) '("a" "b")) 7)
               '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b") (1 . "a"))))
(begin (display "test8 = ") (displayln test8))

(define test91 (equal?
                (vector-assoc 2 (vector '(1 2) 3 '(2 3))) '(2 3)))
(begin (display "test91 = ") (displayln test91))
(define test92 (equal?
                (vector-assoc 3 (vector '(1 2) 3 '(2 3))) #f))
(begin (display "test92 = ") (displayln test92))

(define test10f (caching-assoc (list '(1 2) '(2 3) '(3 4)) 2))
(define test101 (equal? (test10f 1) (assoc 1 (list '(1 2) '(2 3) '(3 4)))))
(begin (display "test101 = ") (displayln test101))
(define test102 (equal? (test10f 2) (assoc 2 (list '(1 2) '(2 3) '(3 4)))))
(begin (display "test102 = ") (displayln test102))
(define test103 (equal? (test10f 3) (assoc 3 (list '(1 2) '(2 3) '(3 4)))))
(begin (display "test103 = ") (displayln test103))
(define test104 (equal? (test10f 4) (assoc 4 (list '(1 2) '(2 3) '(3 4)))))
(begin (display "test104 = ") (displayln test104))

(define a 7)
(define test111 (while-greater (begin (display "e2") 2) do (begin (set! a (- a 1)) (display "e1") a)))
(display "\n")
(define test112 (while-greater (begin (display "e2") 2) do (begin (set! a (- a 1)) (display "e1") a)))

; origin tests
(define nums (sequence 1 0 5))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))
; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-one-only)
  (place-repeatedly (open-window) 0.5 (stream-add-one dan-then-dog) 27))
