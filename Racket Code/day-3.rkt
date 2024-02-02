#lang racket

;; lambda

;; background: this name comes from the lambda calculus,
;; created by Alonzo Church in the 1930s, in order to prove
;; what "computation" is and what computation can and cannot do.

;; Turing also proved the same thing, and also had to define
;; what computation is, and he came up with what we call
;; Turing machines.

(define x (lambda (w) (+ w 2)))
(define y (lambda (a b) (* a b)))
(define grade-adjuster (lambda (boost-amt)
                         (lambda (grade) (+ grade boost-amt))))
(define multiply3 (lambda (a)
                    (lambda (b)
                      (lambda (c)
                        (* a b c)))))

;; length function, but with no name; can't do it yet
;(lambda (lst)
;  (if (empty? lst) 0
;      (recursion?... (+ 1 (rest lst)))))




;; "map"
;; run a function for every element in a list

;; if we had to define it ourselves:
(define (mymap f lst)
  (if (empty? lst) '()
      (cons (f (first lst))
            (mymap f (rest lst)))))

(map length '((1 1 1 1) (1 1) () (1 1 1 1 1)))

(map (lambda (w) (+ w 2)) '(1 3 4 2 2 5 6))
  
;; filter
;; like map, except drops elements from the list that don't produce
;; a true answer from the given function

;; example: keep only values > 5
(filter (lambda (q) (> q 5)) '(1 3 5 2 5 9 3 10 34 3 4))

;; example: assume you have student scores for multiple students,
;; use map&filter to drop all 0 scores for each student
;; input: '((4 4 2 0 0 2) (2 3 3 4 0 2) (4 4 4 4 0 0))

;; what I'm seeing is: (map (filter (lambda ...))
;; this is bad because (filter ...) will happen first before map,
;; yet your goal was to run filter on EACH inside list

;; consider: (filter (map ...)) also has a problem
;; (what do you get from the map that's relevant to filter?)

;; consider: what do we want to do to each inside list?
;; the filter.
;; how do we write that: (filter (lambda (s) (not (= 0 s))) mylist)

;; mistake: (map (filter (lambda (s) (not (= 0 s))) mylist))

;; answer:

(map (lambda (mylist)
       (filter (lambda (s)
                 (not (= 0 s)))
               mylist))
     '((4 4 2 0 0 2) (2 3 3 4 0 2) (4 4 4 4 0 0)))

;; reduce a.k.a. foldl and foldr
;; foldl = fold from the left
;; foldr = fold from the right
;; reducing or folding means collapse a list into a single result (typically)
;;
;; example:
;; - add up all the values:
;;   (foldl + 0.0 '(1 3 4 2))
;;   what happens:
;;   0.0 + (foldl + (1 3 4 2))
;;   0.0 + 1 = 1.0 + (foldl + (3 4 2))
;;   1.0 + 3 = 4.0 + (foldl + (4 2))
;;   4.0 + 4 = 8.0 + (foldl + (2))
;;   8.0 + 2 = 10.0 - done
;; the rule is, foldl needs a function (like +) that accepts
;; TWO inputs: first input will be the folded value SO FAR,
;; the second input will be whatever is next in the list

;; another example: find the max

(foldl (lambda (max-so-far val-from-list)
         (if (> val-from-list max-so-far)
             val-from-list
             max-so-far))
       0 ;; starting value for 'max-so-far'
       '(1 2 3 3 9 2 4 5 7))

;; in-class challenge:
;; given a bunch of student scores, count how many students
;; are passing, where passing means avg score >= 60.0
;; example input: '((80 30 70 70) (90 85 90 90) (30 10 0 5))
;; output: 2
;; rules: don't use sum or avg functions, length is allowed
;; use map, filter, foldl; also don't define any functions
;; besides 'passing' i.e. (define (passing all-scores) ...)

;; first thought: how to use foldl to make a sum of a list of numbers?

;; (foldl + 0.0 '(80 20 70))

(define (passing threshold)
  (lambda (grades)
    (length
     (filter (lambda (g)
               (>= g threshold))
             (map (lambda (w)
                    (/ (foldl + 0.0 w)
                       (length w)))
                  grades)))))

((passing 60.0) '((80 30 70 70) (90 85 90 90) (30 10 0 5)))


;; challenge

;; first, install csv-reading package
;; - go to File menu, then Package Manager, click Update Package List
;; - search for 'csv', choose csv-reading, click install


(require csv-reading)

;; Reads the csv file and convert it into a list
(define data
  (csv->list
   (make-csv-reader
    (open-input-file "C:/Users/hunte/Downloads/hpai-wild-birds-ver2.csv"))))

#| (define (nth lst n)
  (cond ((empty? lst) null)
      (( = n 0) (first lst))
      (#t )
(
|#
;; count the birds and pair it
(define (inc-species species species-counts)
  (let ((species-pair (assoc species species-counts)))
    (if species-pair
        (map (lambda (pair)
               (if (equal? (rest pair) species)
                   (cons species (+ 1 (rest pair)))
                   pair))
             species-counts)
        (cons (cons species 1) species-counts))))

;; counting finding how many the birds happen
(define (count-species data)
  (foldl (lambda (row species-counts)
           (inc-species (list-ref row 4) species-counts))
         '()
         data))

;; percentages
(define (percentages species-counts)
  (let ((total (foldl (lambda (pair sum) (+ sum (rest pair))) 0 species-counts)))
    (map (lambda (pair)
           (cons (first pair) (/ (* 100.0 (rest pair)) total)))
         species-counts)))

;; Get top n species
(define (get-top-species n data)
  (let* ((species-counts (count-species data))
         (percentages (percentages species-counts)))
    (take (sort percentages (lambda (a b) (> (rest a) (rest b)))) n)))

;; Show the top 10 species
(display (get-top-species 10 data))



#|
(require csv-reading)
(define data
  (csv->list
   (make-csv-reader
    (open-input-file "C:/Users/18136/Desktop/LISP/hpai-wild-birds-ver2.csv"))))


(define (add-or-update-species species-count species)
  (if (null? species-count)
      (list (list species 1))
      (let ((species-pair (assoc species species-count)))
        (if species-pair
            (map (lambda (pair)
                   (if (equal? (car pair) species)
                       (list species (+ 1 (rest pair)))
                       pair))
                 species-count)
            (cons (list species 1) species-count)))))

(define (count-species data)
  (foldl (lambda (row species-count)
           (add-or-update-species species-count (list-ref row 4)))
         '()
         data))

(define (convert-to-percentages species-count)
  (let ((total (foldl (lambda (pair sum) (+ sum (cadr pair))) 0 species-count)))
    (map (lambda (pair)
           (list (car pair) (/ (* (cadr pair) 100.0) total)))
         species-count)))

(define (get-top-species data n)
  (let* ((species-count (count-species data))
         (percentages (convert-to-percentages species-count))
         (sorted-species (sort percentages (lambda (a b) (> (cadr a) (cadr b))))))
    (take sorted-species n)))



(display (get-top-species data 10))









(define (count-species data)
  (foldl
    (lambda (row acc)
      (let* ((species (list-ref row 4))
             (count (hash-ref acc species 0)))
        (hash-set acc species (+ count 1))))
    (hash)  ;; <-- change here, using immutable hash
    data))

(define (calculate-percentages counts total)
  (hash-map counts (lambda (species count)
                      (list species (/ (* count 100.0) total)))))

(define (sort-and-take-top-n data n)
  (take (sort data (lambda (a b) (> (second a) (second b)))) n))

(define (top-n-species data n)
  (let* ((counts (count-species data))
         (total (apply + (hash-values counts)))
         (percentages (calculate-percentages counts total)))
    (sort-and-take-top-n percentages n)))

(displayln (top-n-species data 10))



|#




;; challenge task:
;; show the top 10 species (column 5) in terms of occurrence in the data (# rows),
;; and show the % of rows for each of those species, not the actual # of rows.
;; use foldl, map, reduce
;; you can also use dset/dget (like we did above with counting)
;; and you can use sort from Racket
;; example of sort: (sort '(1 3 2 1) <) -> '(1 1 2 3)
;; or more advanced: (sort '((1 6) (3 2) (3 9)) < #:key (lambda (x) (second x)))
