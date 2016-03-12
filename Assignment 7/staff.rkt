;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname staff) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 07, Problem 1
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

(define (add-staff losm m)
  (cond [(empty? losm) (cons m empty)]
        [(>= (staff-member-id m) (staff-member-id (first losm)))
         (cons (first losm) (add-staff (rest losm) m))]
        [(< (staff-member-id m) (staff-member-id (first losm)))
         (cons m losm)]))

(check-expect (add-staff empty (make-staff-member 1 "Ben" "R&D"))
              (list (make-staff-member 1 "Ben" "R&D")))
(check-expect (add-staff staff-list (make-staff-member 10 "Ben" "R&D"))
              (append staff-list (list (make-staff-member 10 "Ben" "R&D"))))
(check-expect (add-staff staff-list (make-staff-member 0 "Ben" "R&D"))
              (append (list (make-staff-member 0 "Ben" "R&D")) staff-list))
(check-expect (add-staff staff-list2 (make-staff-member 3 "Ben" "R&D"))
              (list
               (make-staff-member
                1 "John" "Engineering")
               (make-staff-member 2 "Adam" "R&D")
               (make-staff-member 3 "Ben" "R&D")
               (make-staff-member 4 "Liz" "Finance")
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-member 6 "Suzy" "R&D")
               (make-staff-member 7 "Leslie" "R&D")
               (make-staff-member 8 "Josh" "Engineering")))

(define (update-staff-info losm m)
  (cond [(empty? losm) (cons m empty)]
        [(> (staff-member-id m) (staff-member-id (first losm)))
         (cons (first losm) (update-staff-info (rest losm) m))]
        [(= (staff-member-id m) (staff-member-id (first losm)))
         (cons m (rest losm))]
        [(< (staff-member-id m) (staff-member-id (first losm)))
         (cons m losm)]))
(check-expect (update-staff-info empty (make-staff-member 1 "Ben" "R&D"))
              (list (make-staff-member 1 "Ben" "R&D")))
(check-expect (update-staff-info staff-list (make-staff-member 10 "Ben" "R&D"))
              (append staff-list (list (make-staff-member 10 "Ben" "R&D"))))
(check-expect (update-staff-info staff-list (make-staff-member 0 "Ben" "R&D"))
              (append (list (make-staff-member 0 "Ben" "R&D")) staff-list))
(check-expect (update-staff-info staff-list (make-staff-member 3 "Ben" "R&D"))
              (list
               (make-staff-member
                1 "John" "Engineering")
               (make-staff-member 2 "Adam" "R&D")
               (make-staff-member 3 "Ben" "R&D")
               (make-staff-member 4 "Liz" "Finance")
               (make-staff-member 5 "Anne" "Defence")
               (make-staff-member 6 "Suzy" "R&D")
               (make-staff-member 7 "Leslie" "R&D")
               (make-staff-member 8 "Josh" "Engineering")))

(define (all-staff-info losm)
  (cond [(empty? losm) empty]
        [else (cons
               (string-append (number->string (staff-member-id (first losm))) " " 
                              (staff-member-name (first losm)) " " 
                              (staff-member-dept (first losm)))
               (all-staff-info (rest losm)))]))
(check-expect (all-staff-info staff-list)
              (list "1 John Engineering" "2 Adam R&D" "3 Albert Engineering"
                    "4 Liz Finance" "5 Anne Defence" "6 Suzy R&D" "7 Leslie R&D"
                    "8 Josh Engineering"))
(check-expect (all-staff-info empty) empty)

(define (find-dept losm dep)
  (cond [(empty? losm) 0]
        [(string=? (staff-member-dept (first losm)) dep)
         (add1 (find-dept (rest losm) dep))]
        [else (find-dept (rest losm) dep)]))

(define (count-staff-by-dept losm lodep)
  (cond [(empty? lodep) empty]
        [else (cons (find-dept losm (first lodep))
                    (count-staff-by-dept losm (rest lodep)))]))

(check-expect (count-staff-by-dept (list staff1 staff2 staff3 staff4 staff5 staff6) dept-list)
              (list 2 1 2 0))
(check-expect (count-staff-by-dept empty empty) empty)
(check-expect (count-staff-by-dept staff-list dept-list) (list 3 1 3 0))
(check-expect (count-staff-by-dept staff-list2 dept-list) (list 2 1 3 0))

;;edit
(define (stf->slr stf loslr)
  (cond [(empty? loslr) (make-salary 0 0 0)]
        [(= (staff-member-id stf) (salary-staff-id (first loslr)))
         (first loslr)]
        [else (stf->slr stf (rest loslr))]))

(define (slr-by-dept lostf dep loslr)
  (cond [(empty? lostf) 0]
        [(string=? (staff-member-dept (first lostf)) dep)
         (+ (salary-base (stf->slr (first lostf) loslr)) (salary-bonus (stf->slr (first lostf) loslr))
            (slr-by-dept (rest lostf) dep loslr))]
        [else (slr-by-dept (rest lostf) dep loslr)]))

(define (slr-by-dep lostf lodep loslr)
  (cond [(empty? lodep) empty]
        [else (cons (slr-by-dept lostf (first lodep) loslr)
                    (slr-by-dep lostf (rest lodep) loslr))]))

(define (avg-salary-by-dept lostf loslr lodep)
    (cond [(empty? (count-staff-by-dept lostf lodep)) empty]
          [(not (zero? (first (count-staff-by-dept lostf lodep))))
           (cons (/ (first (slr-by-dep lostf lodep loslr)) (first (count-staff-by-dept lostf lodep)))
                 (avg-salary-by-dept lostf (rest loslr) (rest lodep)))]
          [else (cons 0 (avg-salary-by-dept lostf (rest loslr) (rest lodep)))]))

(check-expect (avg-salary-by-dept staff-list sal-list dept-list) (list 255020/3 0 324250/3 0))


