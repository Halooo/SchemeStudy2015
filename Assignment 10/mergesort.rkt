;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mergesort) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 10, Problem 1
;; ***************************************************
;;

;; (seperatelst lst) seperate a list into 2 parts, if its
;;   odd length, the first half contain more values
;; seperatelst: (listof X) -> (listof X) (listof X)
;; note: sum of length of resultant lists equal to length of lst
(define (seperatelst lst)
  (local [(define lstlen (length lst))
          (define (take lst n)
            (cond [(or (empty? lst) (zero? n)) empty]
                  [else (cons (first lst)
                              (take (rest lst) (sub1 n)))]))
          (define (drop lst n)
            (cond [(or (empty? lst) (zero? n)) lst]
                  [else (drop (rest lst) (sub1 n))]))]
    (list (take lst (round (/ lstlen 2)))
          (drop lst (round (/ lstlen 2))))))



;(seperatelst '(8 3 7 1 2 5 4))
;(seperatelst '(4))
;(seperatelst '())

;; (merge l1 l2 pred) takes 2 lists and merge them together and
;;   merge according to pred
;; merge: (listof Num) (listof Num) -> (listof Num)
;; examples:
(check-expect (merge '(3 2 1) '(6 5 4) <) (list 3 2 1 6 5 4))
(check-expect (merge '(1 2 3) '(6 5 4) >) (list 6 5 4 1 2 3))
(define (merge l1 l2 pred)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [(pred (first l1) (first l2))
         (cons (first l1) (merge (rest l1) l2 pred))]
        [else (cons (first l2) (merge l1 (rest l2) pred))]))

(check-expect (merge '(3 2 1) '(6 5 4) >) (list 6 5 4 3 2 1))
(check-expect (merge '() '(6 5 4) >) (list 6 5 4))
(check-expect (merge '(1 2 3) '() <) (list 1 2 3))
(check-expect (merge '(1) '() <) (list 1))

;; (mergesort lst pred) takes a list and sort it in order which
;;   order indicated by pred.
;; mergesort: (listof X) -> (listof X)
;; require: pred must compare the type given from lst (X)
(define (mergesort lst pred)
  (local [(define lstlen (length lst))]
    (cond [(empty? lst) '()]
          [(= 1 lstlen) lst]
          [(= 2 lstlen) (merge (list (first lst)) (rest lst) pred)]
          [else (merge (mergesort (first (seperatelst lst)) pred)
                       (mergesort (first (rest (seperatelst lst))) pred) pred)])))

(define longlst (build-list 100000 identity))
;(time (length (mergesort longlst <)))
;(check-expect (mergesort longlst <) longlst)
(check-expect (mergesort '(3 5 2 4 2 1 7 9 13) <) (list 1 2 2 3 4 5 7 9 13))