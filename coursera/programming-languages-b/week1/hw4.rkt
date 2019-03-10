
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride ))))

; 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

; 3 
(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (if (= (remainder n (length xs)) 0)
              (car xs)
              (list-nth-mod (cdr xs) (- (remainder n (length xs)) 1))))))

; 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; stream helper function 
(define (make-stream-n n)
  (lambda () (cons n (make-stream-n (+ n 1)))))

; stream helper function 
(define (map-stream f xs)
  (lambda ()
    (cons
     (f(car (xs)))
     (map-stream f (cdr (xs))))))
; 5 
(define (funny-number-stream)
  ((map-stream
    (lambda (n) (if (= (remainder n 5) 0)(- n) n))
    (make-stream-n 1))))

; 6 
(define (dan-then-dog)
  ((map-stream
    (lambda (n) (if (= (remainder n 2) 0) "dan.jpg" "dog.jpg"))
    (make-stream-n 0))))

;7
(define (stream-add-zero s)
  (map-stream
   (lambda (element) (cons 0 element))
   s))
                 
;8
(define (cycle-lists xs ys)
  (map-stream
   (lambda (n) (cons (list-ref xs (remainder n (length xs)))
                     (list-ref ys (remainder n (length ys)))))
   (make-stream-n 0)))
                     
; 9
(define (vector-assoc v vec)
  (letrec
      ([f (lambda (n)
            (if (= (vector-length vec) n)
                #f
                (if (and
                      (pair? (vector-ref vec n))
                      (equal? (car(vector-ref vec n))  v)
                     )
                    (vector-ref vec n)
                    (f (+ n 1)))))])
    (f 0)))

; 10
(define (cached-assoc xs n)
  (let
      ([cache-index 0]
       [cache-vector (make-vector n #f)])
    (lambda (v)
      (let
          ([from-cache (vector-assoc v cache-vector)])
        (if (equal? #f from-cache)
            (let ([result (assoc v xs)])
              (if (equal? result #f)
                  result
                  (begin
                    (vector-set! cache-vector cache-index result)
                    (set! cache-index (remainder (+ cache-index 1) n))
                    result)))
            from-cache)))))


            

     