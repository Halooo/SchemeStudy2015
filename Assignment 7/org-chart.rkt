;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname org-chart) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct supervisor (id subordinates))

;; (everyone oc) takes an oc and returns a list of all the id of people
;;    under the tree (including itself)
;; everyone: Org-Chart -> (listof Num)
(define (everyone oc)
  (cond [(number? oc) (list oc)]
        [else (cons (supervisor-id oc)
                    (sub-lst (supervisor-subordinates oc)))]))
;; (sub-lst lo-oc) takes a list of org-chart (lo-oc) and helps (everyone oc) to produce
;;    result
;; sub-lst: (listof Org-Chart) -> (Anyof Supervisor/Nat)
(define (sub-lst lo-oc)
  (cond [(empty? lo-oc) empty]
        [else (append (everyone (first lo-oc)) (sub-lst (rest lo-oc)))]))

;; (direct-reports oc id) takes an oc and an id and produce the list of people who 
;;    are supervised under given id
;; direct-reports: Org-Chart Nat -> (listof Nat)
;; example
(check-expect (direct-reports 2 2) empty)
(check-expect (direct-reports 3 3) empty)
(check-expect (direct-reports
               (make-supervisor
                1(list 2 (make-supervisor 3 (list 4 (make-supervisor 5 (list 6)))))) 4) empty)
(define (direct-reports oc id)
  (cond [(and (number? oc) (= id oc)) empty]
        [(and (not (number? oc)) (= id (supervisor-id oc))) (rest (everyone oc))]
        [else (direct-reports-lst (supervisor-subordinates oc) id)]))
;; (direct-reports-lst lo-oc id) takes a lo-oc (listof Org-Chart) and an id and helps
;;    direct-reports to produce final result
;; direct-reports-lst: (listof Org-Chart) Nat -> (Anyof Supervisor/Nat)
(define (direct-reports-lst lo-oc id)
  (cond [(empty? lo-oc) empty]
        ;[(and (number? (first lo-oc)) (= (first lo-oc) id)) (direct-reports (first lo-oc) id)]
        [(and (number? (first lo-oc)) (not (= (first lo-oc) id)))
         (direct-reports-lst (rest lo-oc) id)]
        [(not (= id (supervisor-id (first lo-oc)))) (direct-reports-lst (rest lo-oc) id)]
        [else (direct-reports (first lo-oc) id)]))
;; tests:
(check-expect (direct-reports 1 1) empty)

(check-expect (direct-reports (make-supervisor
                               1(list (make-supervisor 2 (list 3 4)) 5
                                      (make-supervisor 6 (list 7)))) 3) empty)
(check-expect (direct-reports (make-supervisor
                               1(list (make-supervisor 2 (list 3 4)) 5
                                      (make-supervisor 6 (list 7)))) 2) (list 3 4))
(check-expect (direct-reports (make-supervisor
                               1(list (make-supervisor 2 (list 3 4)) 5
                                      (make-supervisor 6 (list 7)))) 6) (list 7))


;; (vacation-approval oc id) takes an oc and id and returns a list of ids which are
;;    either direct or indirect supervisors to id
;; vacation-approval: Org-Chart Nat -> (listof Nat)
;; example
(check-expect (vacation-approval (make-supervisor
                                  1(list (make-supervisor 2 (list 3 4)) 5
                                         (make-supervisor 6 (list 7)))) 2) (list 1))
(check-expect (vacation-approval (make-supervisor
                                  1(list (make-supervisor 2 (list 3 4)) 5
                                         (make-supervisor 6 (list 7)))) 6) (list 1))
(define (vacation-approval oc id)
  (cond [(and (number? oc) (= id oc)) (list oc)]
        [(and (not (number? oc)) (= id (supervisor-id oc))) (rest (everyone oc))]
        [(member? id (direct-reports oc (supervisor-id oc)))
         (cons (supervisor-id oc) (v-a-lst (supervisor-subordinates oc) (supervisor-id oc)))]
        [else (v-a-lst (supervisor-subordinates oc) (supervisor-id oc))]))

(define (v-a-lst lst id)
  (cond [(empty? lst) empty]
        [(and (number? (first lst)) (= (first lst) id)) empty]
        [(and (number? (first lst)) (not (= (first lst) id)))
         (v-a-lst (rest lst) id)]
        [(not (member? id (direct-reports (first lst) (supervisor-id (first lst)))))
         (v-a-lst (rest lst) id)]
        [else (vacation-approval (first lst) id)]))

;; tests
(check-expect (vacation-approval 1 5) empty)
(check-expect (vacation-approval (make-supervisor
                                  1(list (make-supervisor 2 (list 3 4)) 5
                                         (make-supervisor 6 (list 7)))) 5) (list 1))
