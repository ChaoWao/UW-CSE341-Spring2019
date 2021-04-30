#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"
   
   ; Tests for Problem 1
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list)) (munit) "racketlist->mupllist test 1")
   (check-equal? (racketlist->mupllist (list (int 3))) (apair (int 3) (munit)) "racketlist->mupllist test 2")
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (munit))) "racketlist->mupllist test 3")
   (check-equal? (racketlist->mupllist (list 1 2 3 4 5)) (apair 1 (apair 2 (apair 3 (apair 4 (apair 5 (munit)))))) "racketlist->mupllist test 4")
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (munit)) (list) "mupllist->racketlist 1")
   (check-equal? (mupllist->racketlist (apair (int 3) (munit))) (list (int 3)) "mupllist->racketlist 2")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (munit)))) (list (int 3) (int 4)) "mupllist->racketlist 3")
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (apair 5 (munit))))) (list (int 3) (int 4) 5) "mupllist->racketlist 4")

   ; Tests for Problem 2
   ; int test
   (check-equal? (eval-exp (int 3)) (int 3) "int test")
   ; var test
   (check-equal? (eval-under-env (var "a") (cons (cons "a" (int 1)) null)) (int 1) "var test")
   ; munit test
   (check-equal? (eval-exp (munit)) (munit) "munit test")
   ; add test
   (check-equal? (eval-exp (add (int 3) (int 4))) (int 7) "add test")
   ; isgreater test
   (check-equal? (eval-exp (isgreater (int 3) (int 4))) (int 0) "isgreater test 1")
   (check-equal? (eval-exp (isgreater (int 3) (int 3))) (int 0) "isgreater test 2")
   (check-equal? (eval-exp (isgreater (int 3) (int 2))) (int 1) "isgreater test 3")
   ; ifnz test
   (check-equal? (eval-exp (ifnz (int 0) (int 1) (int 2))) (int 2) "ifnz test 1")
   (check-equal? (eval-exp (ifnz (int 1) (int 1) (int 2))) (int 1) "ifnz test 2")
   ; fun test
   ; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test 1")
   (check-equal? (eval-exp (mlet "x" (int 1) (munit))) (munit) "mlet test 2")
   ; call test
   (check-equal? (eval-exp (call (fun null "x" (add (var "x") (int 7))) (int 1))) (int 8) "call test 1")
   (check-equal? (eval-exp (call (closure '() (fun null "x" (apair (var "x") (int 7)))) (add (int 1) (int 2)))) (apair (int 3) (int 7)) "call test 2")
   ; apair test
   (check-equal? (eval-exp (apair (int 3) (int 4))) (apair (int 3) (int 4)) "apair test 1")
   (check-equal? (eval-exp (apair (add (int 3) (int 4)) (int 4))) (apair (int 7) (int 4)) "apair test 2")
   (check-equal? (eval-exp (apair (add (int 3) (int 4)) (add (int 1) (int 4)))) (apair (int 7) (int 5)) "apair test 3")
   ; first test
   (check-equal? (eval-exp (first (apair (int 3) (int 4)))) (int 3) "first test")
   ; second test
   (check-equal? (eval-exp (second (apair (int 3) (int 4)))) (int 4) "second test")
   ; ismunit test
   (check-equal? (eval-exp (ismunit (munit))) (int 1) "ismunit test 1")
   (check-equal? (eval-exp (ismunit (apair (int 3) (munit)))) (int 0) "ismunit test 2")
   
   ; Tests for Problem 3
   ; ifmunit test
   (check-equal? (eval-exp (ifmunit (int 1) (int 2) (int 3))) (int 3) "ifmunit test 1")
   (check-equal? (eval-exp (ifmunit (munit) (add (int 2) (int 3)) (int 3))) (int 5) "ifmunit test 2")
   ; mlet* test
   (check-equal? (eval-exp (mlet* (list) (int 10))) (int 10) "mlet* test 1")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test 2")
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10)) (cons "y" (add (int 5) (var "x")))) (var "y"))) (int 15) "mlet* test 3")
   ; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")
   (check-equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3) "ifeq test")
   
   ; Tests for Problem 4

   ; Origin Tests
   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
