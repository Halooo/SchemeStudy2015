;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname tutorclass-notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (insert-bst bst k v)
  (cond [(empty? bst) (make-node k v empty empty)]
        [(= k (node-key bst)) bst]
        [(< k (node-key bst)) (make-node (node-key bst)
                                         (node-val bst)
                                         (insert-bst (node-left bst) k v)
                                         (node-right bst))]
        [(> k (node-key bst)) (make-node (node-key bst)
                                         (node-val bst)
                                         (node-left bst)
                                         (insert-bst (node-right bst) k v))]))

;;2.b sort

;;2.c binary search + selecting
(define (bst-search bst k)
  (cond [(empty? bst) false]
        [(= k (node-key bst)) (node-val bst)]
        [(< k (node-key bst)) (bst-search (node-left bst k))]
        [else (bst-search (node-right bst k))]))


(define (fire lst)
  (cond [(empty? lst) empty]
        []))


;; calculate sum of key in bst
(define (sum-key bst)
  (cond
    [(empty? bst) 0]
    [else (+ (node-key bst) (sum-key (node-left bst))
             (sum-key (node-right bst)))]))

;; 2.d

;; 3.a
(define (everyone oc)
  (cond [(number? oc) (list oc)]
        [else (cons (supervisor-id oc)
                    (everyonelst (supervisor-subordinates oc)))]))

(define (everyonelst lo-oc)
  (cond [(empty? lo-oc) empty]
        [else (append (everyone (first lo-oc)) (everyonelst (rest lo-oc)))]))

(define (direct-reports oc id)
  (cond []
        
(check-expect (direct-reports (make-supervisor
                               1(list 2 (make-supervisor 3 (list 4 5))
                                      (make-supervisor 6 (list 7)))) 7) empty)



;; 3.b









