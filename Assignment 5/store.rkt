;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname store) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 05, Problem 2
;; ***************************************************
;;

(define price-mult-with-tax-rate 1.13)

(define-struct product (name price taxable?))
;; A Product is a (make-product Sym Num Bool)
;; requires: price > 0,
;; price cannot have fractional cents.

;; (have-product? target-prod lst-o-prod) takes a symbol target-prod and
;;    a lst-o-prod return true if target-prod is one of the elements in
;;    lst-o-prod, return false otherwise
;; have-product?: Sym (listof Product) -> Bool
;; Example:
(check-expect (have-product?
               'doughnut (list (make-product 'doughnut 2 true) 
                               (make-product 'panda 1000 false)
                               (make-product 'cards 5 true)))
              true)
(check-expect (have-product?
               'doughnut (list (make-product 'banana 1 true) 
                               (make-product 'apple 3 true)
                               (make-product 'cherry 7 true)))
              false)

(define (have-product? target-prod prod-lst)
  (cond [(empty? prod-lst) false] 
        [(symbol=? target-prod
                   (product-name (first prod-lst))) true]
        [else (have-product? target-prod (rest prod-lst))]))

;; Test:
(check-expect (have-product?
               'cards empty) false)

(check-expect (have-product?
               'banana (list (make-product 'apple 3 true)
                             (make-product 'banana 5 true)))
              true)

;; (valid-order? target-lst prod-lst) takes a target-lst and a prod-list and
;;    determine if all the elements in target-lst matches the name of product
;;    in the prod-lst, return false if they do not match
;; valid-order?: (listof Sym) (listof Product) -> Bool
;; Example:
(check-expect (valid-order? (list 'a 'b 'c)
                            (list (make-product 'a 1 true) 
                                  (make-product 'b 3 true)
                                  (make-product 'c 7 true))) true)
(check-expect (valid-order? (list 'a 'b 'c)
                            (list (make-product 'a 1 true) 
                                  (make-product 'c 3 true)
                                  (make-product 'c 7 true))) false)

(define (valid-order? target-lst prod-lst)
  (cond [(empty? target-lst) true]
        [(and (have-product? (first target-lst) prod-lst)
              (valid-order? (rest target-lst) prod-lst)) true]
        [else false]))

;; test:
(check-expect (valid-order? empty empty) true)
(check-expect (valid-order? empty
                            (list (make-product 'a 2 true) 
                                  (make-product 'b 3 true)
                                  (make-product 'c 7 true))) true)
(check-expect (valid-order? (list 'a 'b)
                            (list (make-product 'a 1 true) 
                                  (make-product 'd 3 true)
                                  (make-product 'c 7 true))) false)
(check-expect (valid-order? (list 'a 'b)
                            (list (make-product 'a 1 true) 
                                  (make-product 'b 3 true)
                                  (make-product 'b 7 true))) true)
(check-expect (valid-order? (list 'a 'b 'c)
                            empty) false)

;; (budget-items prod-lst lim) takes a prod-lst (list of Products) and a lim
;;    as price limit. It returns a list of the products less or equal to
;;    price lim. note the price should be after tax(13%)
;; budget-items: (listof Product) Num -> (listof Product)
;; examples:
(check-expect (budget-items
               (list (make-product 'a 10 true) 
                     (make-product 'd 3 true)
                     (make-product 'c 7 true))
               10)
              (list (make-product 'd 3 true)
                    (make-product 'c 7 true)))
(check-expect (budget-items
               (list (make-product 'a 10 false) 
                     (make-product 'd 9.8 false)
                     (make-product 'c 8.5 true))
               10)
              (list (make-product 'a 10 false) 
                    (make-product 'd 9.8 false)
                    (make-product 'c 8.5 true)))

(define (budget-items prod-lst lim)
  (cond [(empty? prod-lst) empty]
        [(or (and (product-taxable? (first prod-lst)) 
                  (<= (* price-mult-with-tax-rate
                         (product-price (first prod-lst))) lim))
             (and (not (product-taxable? (first prod-lst)))
                  (<= (product-price (first prod-lst)) lim)))
         (cons (first prod-lst) (budget-items (rest prod-lst) lim))]
        [else (budget-items (rest prod-lst) lim)]))

;; test:
(check-expect (budget-items empty 10) empty)

(check-expect (budget-items
               (list (make-product 'a 10 false) 
                     (make-product 'd 9.8 false)
                     (make-product 'c 8.5 true)) 0) empty)

(check-expect (budget-items
               (list (make-product 'a 10 true) 
                     (make-product 'b 20 false)
                     (make-product 'e 88 true)) 100)
              (list (make-product 'a 10 true) 
                    (make-product 'b 20 false)
                    (make-product 'e 88 true)))

(check-expect (budget-items
               (list (make-product 'a 17 true) 
                     (make-product 'b 23 false)
                     (make-product 'e 89 true)) 100)
              (list (make-product 'a 17 true) 
                    (make-product 'b 23 false)))

;; (target-finder sym1 prod-lst) take a sym1 and a prod-lst and return the
;;    product that has the same name as the symbol
;; target-finder: Sym (listof Products) -> Product
;; example:
(check-expect (target-finder 'a empty) (make-product 'nil-prod 0 true))
(check-expect (target-finder 'a (list (make-product 'a 17 true)))
              (make-product 'a 17 true))
(check-expect (target-finder 'b (list (make-product 'a 17 true)
                                      (make-product 'b 23 false)))
              (make-product 'b 23 false))
(define (target-finder sym1 prod-lst)
  (cond [(empty? prod-lst) (make-product 'nil-prod 0 true)]
        [(symbol=? sym1 (product-name (first prod-lst))) (first prod-lst)]
        [else (target-finder sym1 (rest prod-lst))]))

;; (help-total-order target-lst prod-lst) takes a target-lst and a prod-lst, and
;;    calculate the amount need to be payed in order to purchase all items
;;    listed in target-lst. It gives 'cannot-fulfill if the order is not valid
;;    This returns the value without rounding 
;; total-order: (listof Sym) (listof Product) -> Num
;; example:
(check-expect (help-total-order (list 'a 'b 'c)
                           (list (make-product 'a 10 false) 
                                 (make-product 'b 20 false)
                                 (make-product 'c 100 false))) 130)
(check-expect (help-total-order (list 'a 'b 'c)
                           (list (make-product 'a 11.35 true) 
                                 (make-product 'b 20 false)
                                 (make-product 'c 88.95 true))) 133.339)
(define (help-total-order target-lst prod-lst)
  (cond [(not (valid-order? target-lst prod-lst)) 'cannot-fulfill]
        [(empty? target-lst) 0]
        [(product-taxable? (target-finder (first target-lst) prod-lst))
         (+ (* price-mult-with-tax-rate 
               (product-price (target-finder (first target-lst) prod-lst)))
            (help-total-order (rest target-lst) prod-lst))]
        [(not (product-taxable? (target-finder (first target-lst) prod-lst)))
         (+ (product-price (target-finder (first target-lst) prod-lst))
            (help-total-order (rest target-lst) prod-lst))]))

  
;; Test:
(check-expect (help-total-order (list 'a 'b 'c)
                           empty) 'cannot-fulfill)
(check-expect (help-total-order (list 'a 'b 'c)
                           (list (make-product 'a 10 true) 
                                 (make-product 'b 20 false)
                                 (make-product 'c 100 true))) 144.3)

;; (total-order target-lst prod-lst) takes a target-lst and a prod-lst, and
;;    calculate the amount need to be payed in order to purchase all items
;;    listed in target-lst. It gives 'cannot-fulfill if the order is not valid
;;    Resaltant price is rounded down to 2 decimal places
;; total-order: (listof Sym) (listof Product) -> Num
;; Example:
(check-expect (total-order (list 'a 'b 'c)
                           (list (make-product 'a 10 false) 
                                 (make-product 'b 20 false)
                                 (make-product 'c 100 false))) 130)
(check-expect (total-order (list 'a 'b 'c)
                           (list (make-product 'a 11.35 true) 
                                 (make-product 'b 20 false)
                                 (make-product 'c 88.95 true))) 133.33)

(define (total-order target-lst prod-lst)
  (/ (floor (* (help-total-order target-lst prod-lst) 100)) 100))

;; Test:
(check-expect (help-total-order (list 'a 'b 'c)
                           empty) 'cannot-fulfill)

(check-expect (total-order (list 'a 'b)
                           (list (make-product 'a 11.35 true) 
                                 (make-product 'b 20 false)
                                 (make-product 'c 88.95 true))) 32.82)

  
