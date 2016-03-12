;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname patient) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;; Hao Sun(20611135)
;; CS 135 Fall 2015
;; Assignment 06, Problem 4
;; ***************************************************
;;

(define sys-low 70)
(define sys-high 150)
(define oxygen-lim 90)
(define temp-low 95)
(define temp-high 104)
(define bpsys-multiplier 0.5)

(define-struct patient-profile (name bp oxy tempf))
;; A Patient-Profile is a (list Str Int Num Num)

(define (bpsys p1)
  (cond [(< (patient-profile-bp p1) sys-low) (- sys-low (patient-profile-bp p1))]
        [(> (patient-profile-bp p1) sys-high) (- (patient-profile-bp p1) sys-high)]
        [(and (>= (patient-profile-bp p1) sys-low)
              (<= (patient-profile-bp p1) sys-high)) 0]))

(define (oxy-lvl p1)
  (cond [(< (patient-profile-oxy p1) oxygen-lim) (expt 2 (- oxygen-lim (patient-profile-oxy p1)))]
        [else 0]))

(define (tempf-lvl p1)
  (cond [(< (patient-profile-tempf p1) temp-low) (- temp-low (patient-profile-tempf p1))]
        [(> (patient-profile-tempf p1) temp-high) (- (patient-profile-tempf p1) temp-high)]
        [(and (>= (patient-profile-tempf p1) temp-low)
              (<= (patient-profile-tempf p1) temp-high)) 0]))

;; example
(check-expect (patient-severity (make-patient-profile "Armghan" 65 85 105)) 35.5)
(check-expect (patient-severity (make-patient-profile "Ben" 120 90 120)) 16)

(define (patient-severity profile)
  (+ (* bpsys-multiplier (bpsys profile))
     (oxy-lvl profile) (tempf-lvl profile)))

;; test
(check-expect (patient-severity (make-patient-profile "Cart" 1000 2000 3000)) 3321)
(check-expect (patient-severity (make-patient-profile "Dan" 110 100 140)) 36)
(check-expect (patient-severity (make-patient-profile "Elan" 120 90 100)) 0)
(check-expect (patient-severity (make-patient-profile "Fred" 900 90 120)) 391)
(check-expect (patient-severity (make-patient-profile "Grabby" 270 1944 21)) 134)


;; A Priority-List is a (listof (list Num Patient-Profile))

;; some-priority-list: Priority-List -> (listof Num Patient-Profile)
(define (some-priority-list lst)
  (cond [(empty? (some-priority-list lst)) ...]
        [else (... (first (first lst)) ...
              (rest (first lst))...
              (rest lst)...)]))

(define (insert-priority-list prio-list profile)
  (cond [(empty? prio-list) (cons profile empty)]
        [(>= (patient-severity profile) (first (first prio-list)))
         (cons (patient-severity profile) prio-list)]
        [else (cons
               (first prio-list) (insert-priority-list
                                  (patient-severity profile)
                                  (rest prio-list)))]))
