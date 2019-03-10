#lang racket
;; Programming Languages Homework 5 Simple Test
;; Save this file to the same directory as your homework file
;; These are basic tests. Passing these tests does not guarantee that your code will pass the actual homework grader

;; Be sure to put your homework file in the same folder as this test file.
;; Uncomment the line below and, if necessary, change the filename
;;(require "hw5")

(require rackunit)
(require "hw5.rkt")


;

;(define curry (closure null
;                       (fun "fa" "x" (closure null (fun "fb" "y" (add (var "x") (var "y")))))))
;(eval-exp (call (call curry (int 1)) (int 2)))
;(eval-exp (call curry (int 1)))
;(eval-exp (call (closure null
 ;                      (fun "fa" "x" (closure null (fun "fb" "y" (add (var "x") (var "y")))))) (int 1)))


;(define ca (eval-exp (call mupl-map (fun #f "x" (add (var "x") (int 7))))))
;(eval-exp (call ca (aunit)))
;(eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))

;(eval-exp (call mupl-map (fun #f "x" (add (var "x") (int 7)))))
;(eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit))))
;(eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (apair (int 2) (aunit)))))

(define l (apair (int 1) (apair (int 2) (aunit))))
(define f (closure null (fun "df" "x" (fst (var "x")))))

(eval-exp (call (fun #f "x" (add (var "x") (int 2))) (int 4)))

(define tests
  (test-suite
   "Sample tests for Assignment 5"
   
   ;; check racketlist to mupllist with normal list
   (check-equal? (racketlist->mupllist (list (int 3) (int 4))) (apair (int 3) (apair (int 4) (aunit))) "racketlist->mupllist test")
   
   ;; check mupllist to racketlist with normal list
   (check-equal? (mupllist->racketlist (apair (int 3) (apair (int 4) (aunit)))) (list (int 3) (int 4)) "racketlist->mupllist test")


   (check-equal? (eval-exp (int 1)) (int 1))
   (check-equal? (eval-exp (add (int 1) (int 2))) (int 3))
   (check-equal? (eval-exp (add (int 1) (add (int 1) (int 1)))) (int 3))
   
   (check-equal? (eval-exp (apair (int 1) (int 2))) (apair (int 1) (int 2 )))
   (check-equal? (eval-exp (fst (apair (int 1) (int 2)))) (int 1))
   ;; tests if ifgreater returns (int 2)
   (check-equal? (eval-exp (ifgreater (int 3) (int 4) (int 3) (int 2))) (int 2) "ifgreater test")
   
   ;; mlet test
   (check-equal? (eval-exp (mlet "x" (int 1) (add (int 5) (var "x")))) (int 6) "mlet test")
   
   ;; call test
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x") (int 7)))) (int 1))) (int 8) "call test")
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x")
                                                               (call (closure '() (fun #f "x" (add (var "x") (int 1)))) (int 1))
                                                               ))) (int 7))) (int 9))
   (check-equal? (eval-exp (call (closure '() (fun #f "x" (add (var "x")
                                                               (call (closure '() (fun #f "x" (add (var "x") (int 1)))) (int 1))
                                                               ))) (int 7))) (int 9))
   (check-equal? (eval-exp (call f l)) (int 1))
   (check-equal? (eval-exp (call (fun "sum-to" "n" (ifgreater (var "n") (int 1)
                                       (add (var "n")
                                            (call (var "sum-to")
                                                  (add (var "n") (int -1))))
                                       (int 1)))
          (int 5))) (int 15))
   
   ;;snd test
   (check-equal? (eval-exp (snd (apair (int 1) (int 2)))) (int 2) "snd test")
   
   ;; isaunit test
   (check-equal? (eval-exp (isaunit (closure '() (fun #f "x" (aunit))))) (int 0) "isaunit test")


   
   ;; ifaunit test
   (check-equal? (eval-exp (ifaunit (int 1) (int 2) (int 3))) (int 3) "ifaunit test")
   
   ;; mlet* test
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))) (var "x"))) (int 10) "mlet* test")
   (check-equal? (eval-exp (mlet* null (int 1))) (int 1))
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))
                                        (cons "x" (int 20))) (var "x"))) (int 20) "mlet* test")
   
   (check-equal? (eval-exp (mlet* (list (cons "x" (int 10))
                                        (cons "y" (int 20))) (add (var "x") (var "y")))) (int 30) "mlet* test")
   ;; ifeq test
   (check-equal? (eval-exp (ifeq (int 1) (int 2) (int 3) (int 4))) (int 4) "ifeq test")

  
   ;; mupl-map test
   (check-equal? (eval-exp (call (call mupl-map (fun #f "x" (add (var "x") (int 7)))) (apair (int 1) (aunit)))) 
                 (apair (int 8) (aunit)) "mupl-map test")
   
   ;; problems 1, 2, and 4 combined test
   (check-equal? (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))) (list (int 10) (int 11) (int 16)) "combined test")
   
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
