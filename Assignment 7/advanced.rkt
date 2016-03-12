;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname advanced) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 07, Problem 2
;; ***************************************************
;;

;; A Department-List is a (listof Str)
(define-struct staff-member (id name dept))
;; A Staff-Member is a (make-staff-member Nat Str Str)
;; requires: id is unique
;; (i.e., every staff-member with the same id also has the same name)
(define-struct salary (staff-id base bonus))
;; A Salary is a (make-salary Nat Num Num)
;; requires: base, bonus ≥ 0
;; A Staff-List is a (listof Staff-Member)
;; requires: elements are sorted by increasing id
;; A Salary-List is a (listof Salary)

(define-struct staff-node (staff left right))
;; A Staff-Node is a (make-staff-node Staff-Member Staff-BST Staff-BST)
;; requires: ids of all Staff-Member on the left subtree are smaller than the id of Staff-Member
;; ids of all Staff-Member on the right subtree are larger than the id of Staff-Member
;; A Staff-BST is one of:
;;* empty
;;* Staff-Node

(define staff1 (make-staff-member 1 "John" "Engineering"))
(define staff2 (make-staff-member 2 "Adam" "R&D"))
(define staff3 (make-staff-member 3 "Albert" "Engineering"))
(define staff4 (make-staff-member 4 "Liz" "Finance"))
(define staff5 (make-staff-member 5 "Anne" "Defence"))
(define staff6 (make-staff-member 6 "Suzy" "R&D"))
(define staff7 (make-staff-member 7 "Leslie" "R&D"))
(define staff8 (make-staff-member 8 "Josh" "Engineering"))
(define salary1 (make-salary 1 50000 10))
(define salary2 (make-salary 2 74000 250))
(define salary3 (make-salary 3 85000 10))
(define salary4 (make-salary 4 60000 0))
(define salary5 (make-salary 5 93000 100))
(define salary6 (make-salary 6 68500 0))
(define salary7 (make-salary 7 250000 0))
(define salary8 (make-salary 8 120000 0))
(define staff-list (list staff1 staff2 staff3 staff4 staff5 staff6 staff7 staff8))
(define sal-list (list salary1 salary2 salary3 salary4 salary7 salary8))
(define dept-list (list "Engineering" "Defence" "R&D" "Management"))
(define staff-list2 (list staff1 staff2 staff4 staff5 staff6 staff7 staff8))
(define sal-list2 (list salary1 salary2 salary3 salary4 salary5 salary6 salary7 salary8))
;; (add-staff-bst stf-t stf) takes an identical stf and a stf-t and add the stf into the stf-t
;; add-staff-bst: Staff-BST Staff -> Staff-BST
;; example:
(check-expect (add-staff-bst (make-staff-node staff1 '() '()) staff3)
              (make-staff-node staff1 '() (make-staff-node staff3 '() '())))
(check-expect (add-staff-bst (make-staff-node staff2 '() '()) staff3)
              (make-staff-node staff2 '() (make-staff-node staff3 '() '())))

(define (add-staff-bst stf-t stf)
  (cond [(empty? stf-t) (make-staff-node stf empty empty)]
        [(= (staff-member-id stf)
            (staff-member-id (staff-node-staff stf-t))) stf-t]
        [(< (staff-member-id stf)
            (staff-member-id (staff-node-staff stf-t)))
         (make-staff-node (staff-node-staff stf-t)
                          (add-staff-bst (staff-node-left stf-t) stf)
                          (staff-node-right stf-t))]
        [(> (staff-member-id stf)
            (staff-member-id (staff-node-staff stf-t)))
         (make-staff-node (staff-node-staff stf-t)
                          (staff-node-left stf-t)
                          (add-staff-bst (staff-node-right stf-t) stf))]))
;; tests:
(check-expect (add-staff-bst (make-staff-node staff1 '() '()) staff1) (make-staff-node staff1 '() '()))
(check-expect (add-staff-bst (make-staff-node staff1 '() '()) staff2)
              (make-staff-node staff1 '() (make-staff-node staff2 '() '())))
(check-expect (add-staff-bst (make-staff-node staff2 '() (make-staff-node staff3 '() '())) staff1)
              (make-staff-node staff2 (make-staff-node staff1 '() '()) (make-staff-node staff3 '() '())))

;; (create-staff-bst-from-list lostf) takes a lostf list of staff and make a binary search tree with it
;;    the order is by its staff id
;; create-staff-bst-from-list: (listof Staff) -> Staff-BST
;; example
(check-expect (create-staff-bst-from-list (list staff1 staff2))
              (make-staff-node
               (make-staff-member 2 "Adam" "R&D")
               (make-staff-node (make-staff-member 1 "John" "Engineering") empty empty)
               empty))
(check-expect (create-staff-bst-from-list (list staff1 staff2))
              (make-staff-node
               (make-staff-member 2 "Adam" "R&D")
               (make-staff-node (make-staff-member 1 "John" "Engineering") empty empty)
               empty))
(check-expect (create-staff-bst-from-list (list staff3 staff2))
              (make-staff-node
               (make-staff-member 2 "Adam" "R&D")
               empty
               (make-staff-node (make-staff-member 3 "Albert" "Engineering") empty empty)))

(define (create-staff-bst-from-list lostf)
  (cond [(empty? lostf) empty]
        [else (add-staff-bst 
               (create-staff-bst-from-list (rest lostf))
               (first lostf))]))

;; tests: 
(check-expect (create-staff-bst-from-list (list staff7 staff4 staff1))
              (make-staff-node
               (make-staff-member 1 "John" "Engineering")
               empty
               (make-staff-node
                (make-staff-member 4 "Liz" "Finance")
                empty
                (make-staff-node (make-staff-member 7 "Leslie" "R&D") empty empty))))
(check-expect (create-staff-bst-from-list (list staff1 staff2 staff3))
              (make-staff-node
               (make-staff-member 3 "Albert" "Engineering")
               (make-staff-node
                (make-staff-member 2 "Adam" "R&D")
                (make-staff-node (make-staff-member 1 "John" "Engineering") empty empty)
                empty)
               empty))
(check-expect (create-staff-bst-from-list
               (list staff3 staff6 staff7 staff8))
              (make-staff-node
               (make-staff-member 8 "Josh" "Engineering")
               (make-staff-node
                (make-staff-member 7 "Leslie" "R&D")
                (make-staff-node
                 (make-staff-member 6 "Suzy" "R&D")
                 (make-staff-node (make-staff-member 3 "Albert" "Engineering") empty empty)
                 empty)
                empty)
               empty))
;; (bst-search slr staff-bst) searches the staff-bst by given slr id
;;    and gives the total salary of coresponding staff in the staff-bst
;; bst-search: Salary Staff-BST -> Num
(define (bst-search slr staff-bst)
  (cond [(empty? staff-bst) 0]
        [(= (salary-staff-id slr) 
            (staff-member-id (staff-node-staff staff-bst)))
         (+ (salary-base slr) (salary-bonus slr))]
        [(< (salary-staff-id slr)
            (staff-member-id (staff-node-staff staff-bst)))
         (bst-search slr (staff-node-left staff-bst))]
        [else (bst-search slr (staff-node-right staff-bst))]))
;; (get-staff id stf-bst) takes an id and a stf-bst and return the staff with coresponding id
;; get-staff: Num Staff-BST -> Staff
(define (get-staff id stf-bst)
  (cond [(empty? stf-bst) false]
        [(= id (staff-member-id (staff-node-staff stf-bst)))
         (staff-node-staff stf-bst)]
        [(< id (staff-member-id (staff-node-staff stf-bst)))
         (get-staff id (staff-node-left stf-bst))]
        [else (get-staff id (staff-node-right stf-bst))]))
;; (who-to-fire loslr stf-bst n) takes a list of salary loslr, a staff-BST stf-bst and
;;    a number n, returns a list of staff who's in the stf-bst earn total amount greater
;;    than n.
;; who-to-fire: (listof Salary) Staff-BST Num -> (listof Staff)
;; example:
(define s-bst1 (create-staff-bst-from-list staff-list2))
(define s-bst2 (create-staff-bst-from-list staff-list))
(check-expect (who-to-fire sal-list s-bst2 120000)
              (list (make-staff-member 7 "Leslie" "R&D")))
(check-expect (who-to-fire sal-list2 s-bst2 70000)
              (list
               (make-staff-member 2 "Adam" "R&D")
               (make-staff-member 3 "Albert" "Engineering")
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-member 7 "Leslie" "R&D")
               (make-staff-member 8 "Josh" "Engineering")))

(define (who-to-fire loslr stf-bst n)
  (cond [(empty? loslr) empty]
        [(< n (bst-search (first loslr) stf-bst))
         (cons (get-staff (salary-staff-id (first loslr)) stf-bst)
               (who-to-fire (rest loslr) stf-bst n))]
        [else (who-to-fire (rest loslr) stf-bst n)]))

;; tests:
(check-expect (who-to-fire sal-list s-bst2 50000)
              (list
               (make-staff-member 1 "John" "Engineering")
               (make-staff-member 2 "Adam" "R&D")
               (make-staff-member 3 "Albert" "Engineering")
               (make-staff-member 4 "Liz" "Finance")
               (make-staff-member 7 "Leslie" "R&D")
               (make-staff-member 8 "Josh" "Engineering")))

(check-expect (who-to-fire sal-list2 s-bst2 50000)
              (list
               (make-staff-member 1 "John" "Engineering")
               (make-staff-member 2 "Adam" "R&D")
               (make-staff-member 3 "Albert" "Engineering")
               (make-staff-member 4 "Liz" "Finance")
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-member 6 "Suzy" "R&D")
               (make-staff-member 7 "Leslie" "R&D")
               (make-staff-member 8 "Josh" "Engineering")))

(check-expect (who-to-fire sal-list2 s-bst1 80000)
              (list
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-member 7 "Leslie" "R&D")
               (make-staff-member 8 "Josh" "Engineering")))
(check-expect (who-to-fire empty empty 80000) empty)

;; (remove-root-bst) removes the root key from the stf-bst
;; remove-root-bst: Staff-BST -> Staff-BST
(define (remove-root-bst stf-bst)
  (cond [(empty? stf-bst) empty]
        [(and (empty? (staff-node-left stf-bst))
              (empty? (staff-node-right stf-bst))) empty]
        [(empty? (staff-node-left stf-bst)) (staff-node-right stf-bst)]
        [(empty? (staff-node-right stf-bst)) (staff-node-left stf-bst)]
        [else (make-staff-node (staff-node-staff (staff-node-right stf-bst))
                               (staff-node-left stf-bst)
                               (remove-root-bst (staff-node-right stf-bst)))]))
;; (remove-from-bst stf-bst id) removes the staff with coresponding id from the stf-bst
;; remove-from-bst: Staff-BST Nat -> Staff-BST
;; example:
(define s-bst3 (create-staff-bst-from-list (list staff1 staff3 staff5 staff7)))
(define s-bst4 (create-staff-bst-from-list (list staff2 staff4 staff6 staff8)))
(define (remove-from-bst stf-bst id)
  (cond [(empty? stf-bst) empty]
        [(= id (staff-member-id (staff-node-staff stf-bst)))
         (remove-root-bst stf-bst)]
        [(< id (staff-member-id (staff-node-staff stf-bst)))
         (make-staff-node (staff-node-staff stf-bst)
                          (remove-from-bst id (staff-node-left stf-bst))
                          (staff-node-right stf-bst))]
        [else
         (make-staff-node (staff-node-staff stf-bst)
                          (staff-node-left stf-bst)
                          (remove-from-bst id (staff-node-right stf-bst)))]))
