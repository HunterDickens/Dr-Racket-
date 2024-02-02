#lang racket

(define(mymap f list)
  (if (empty? list) '()
  (cons ( f (first list))
        (mymap f (rest lst))))


;;map
;; run a runciuon for every elecment in a list
(map length '(( 1 1 1 1) (1 1) () (1 1 1 1 1 1)))
  (map (lambda (w) (+ w 2)) '(1 2 3 4 5 6))



;; filter
;; like map, crops elemts from the list that dont produece a trucer anser

  (filter (lambda (a) (> a5)) '(1 2 3 4 5 10 12))

  ;; Example: assume you have student scores for multiple students

  (

  