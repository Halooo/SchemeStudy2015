;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname applicants) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 05, Problem 3
;; ***************************************************
;;

(define-struct pprof (title wts))
;; A position profile (PProf) is a (make-pprof Sym (listof Nat))
;; N could be 0

;; applicant-score: (listof Nat) PProf -> Nat
;; require: the number of skill must has the same number of elements
;;    as profile's wts
(define (applicant-score skill-rank some-pprof)
  (cond [(empty? skill-rank) 0]
        [else (+ (* (first skill-rank)
                    (first (pprof-wts some-pprof)))
                 (applicant-score (rest skill-rank)
                                  (rest (pprof-wts some-pprof))))]))

;; Test:

(define (position-max skill-rank pprof1 pprof2)
  (cond [(< (applicant-score skill-rank pprof1)
            (applicant-score skill-rank pprof2)) pprof2]
        [else pprof1]))

;; test:

(define (position-list-max skill-rank pprof-lst)
  (cond [(empty? pprof-lst) empty]
        [(= (length pprof-lst) 1) (first pprof-lst)]
        [else (position-max
               skill-rank
               (position-max skill-rank
                             (first pprof-lst) (first (rest pprof-lst)))
               (rest pprof-lst))]))

;; (remove-position posi-ttl pprof-lst) take a sym1 and a prod-lst and return the
;;    product that has the same name as the symbol
;; remove-position: Sym (listof PProf) -> (listof PProf)
;; example:
(check-expect (remove-position 'cleaner
                               (list (make-pprof 'cleaner (list 1 2 3 4))))
              empty)
(check-expect (remove-position 'cleaner
                               (list (make-pprof 'cleaner (list 1 2 3 4))
                                     (make-pprof 'manager (list 1 2 3 4))
                                     (make-pprof 'cook (list 0 0 0 0))))
              (list (make-pprof 'manager (list 1 2 3 4))
                    (make-pprof 'cook (list 0 0 0 0))))
(define (remove-position posi-ttl pprof-lst)
  (cond [(empty? pprof-lst) empty]
        [(symbol=? posi-ttl (pprof-title (first pprof-lst)))
         (remove-position posi-ttl (rest pprof-lst))]
        [else (cons (first pprof-lst) (remove-position posi-ttl (rest pprof-lst)))]))

;; test
(check-expect (remove-position 'a empty) empty)
(check-expect (remove-position 'manager
                               (list (make-pprof 'cleaner (list 1 2 3 4))
                                     (make-pprof 'manager (list 5 5 5 4))
                                     (make-pprof 'cook (list 0 0 0 0))))
              (list (make-pprof 'cleaner (list 1 2 3 4))
                    (make-pprof 'cook (list 0 0 0 0))))