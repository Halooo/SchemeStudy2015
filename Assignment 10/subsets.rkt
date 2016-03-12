;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 10, Problem 3
;; ***************************************************
;;

;; (subsets1 lon) takes a lon and return all its subsets
;; subsets1: (listof Num) -> (listof (listof Num))
(define (subsets1 lon)
  (local [(define (addelement lon e)
            (cond [(empty? lon) empty]
                  [else (cons (cons e (first lon))
                              (addelement (rest lon) e))]))]
    (cond [(empty? lon) (list empty)]
          [else (append (subsets1 (rest lon))
                        (addelement (subsets1 (rest lon)) (first lon)))])))

(check-expect (subsets2 '(1 2 3))
              (list (list 1 2 3) (list 1 2) (list 1 3) (list 1)
                    (list 2 3) (list 2) (list 3) '()))

;; (subsets2 lon) takes a lon and return all its subsets
;; subsets2: (listof Num) -> (listof (listof Num))
(define (subsets2 lon)
  (foldr (lambda (x y)
           (append (map (lambda (z) (cons x z)) y) y)) (list empty) lon))


(check-expect (subsets2 '(1 2 3))
              (list (list 1 2 3) (list 1 2) (list 1 3) (list 1)
                    (list 2 3) (list 2) (list 3) '()))


;; Bellow is my 'hard try' for subsets3, but failed.
;; problem occurs while trying to build Y combinator for funcs consuming 2/more variables

#|(define (subsets3 lon)
  ((lambda (self lon) (cond [(empty? lon) empty]
                            [else ((lambda (self l1 l2) (cond [(empty? l1) l2]
                                                              [else (cons (first l1) (self self (rest l1)))]))
                                   (lambda (self l1 l2) (cond [(empty? l1) l2]
                                                              [else (cons (first l1) (self self (rest l1)))])) (self self (rest lon))
                                                                                                               ((lambda (self lon e) (cond [(empty? lon) empty]
                                                                                                                                           [else (cons (cons e (first lon))
                                                                                                                                                       (self self (rest lon) e))]))
                                                                                                                (lambda (self lon e) (cond [(empty? lon) empty]
                                                                                                                                           [else (cons (cons e (first lon))
                                                                                                                                                       (self self (rest lon) e))]))
                                                                                                                (self self (rest lon)) (first lon)))
                                                                                                               ]))
   
   (lambda (self lon) (cond [(empty? lon) empty]
                            [else ((lambda (self l1 l2) (cond [(empty? l1) l2]
                                                              [else (cons (first l1) (self self (rest l1)))]))
                                   (lambda (self l1 l2) (cond [(empty? l1) l2]
                                                              [else (cons (first l1) (self self (rest l1)))])) (self self (rest lon))
                                                                                                               ((lambda (self lon e) (cond [(empty? lon) empty]
                                                                                                                                           [else (cons (cons e (first lon))
                                                                                                                                                       (self self (rest lon) e))]))
                                                                                                                (lambda (self lon e) (cond [(empty? lon) empty]
                                                                                                                                           [else (cons (cons e (first lon))
                                                                                                                                                       (self self (rest lon) e))]))
                                                                                                                (self self (rest lon)) (first lon)))
                                                                                                               ]))
   lon))

(check-expect (subsets3 '(1 2 3))
              (list (list 1 2 3) (list 1 2) (list 1 3) (list 1)
                    (list 2 3) (list 2) (list 3) '()))|#

#|
; append
((lambda (self l1 l2) (cond [(empty? l1) l2]
                            [else (cons (first l1) (self self (rest l1)))]))
 (lambda (self l1 l2) (cond [(empty? l1) l2]
                            [else (cons (first l1) (self self (rest l1)))]))
 (first list) (self self (rest list)))

;addelement
((lambda (self lon e) (cond [(empty? lon) empty]
                            [else (cons (cons e (first lon))
                                        (self self (rest lon) e))]))
 (lambda (self lon e) (cond [(empty? lon) empty]
                            [else (cons (cons e (first lon))
                                        (self self (rest lon) e))]))
 lon e)|#