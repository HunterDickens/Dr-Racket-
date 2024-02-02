#lang racket

#| lambda
background: this name comes from the lambda calcuulus
created by alonzo church proved what computation is and what it can do 








(define(mymap f list)
  (if (empty? list) '()
  (cons ( f (first list))
        (mymap f (rest list)))))


;;map
;; run a runciuon for every elecment in a list
(map length '(( 1 1 1 1) (1 1) () (1 1 1 1 1 1)))
  (map (lambda (w) (+ w 2)) '(1 2 3 4 5 6))



;; filter
;; like map, crops elemts from the list that dont produece a trucer anser

  (filter (lambda (a) (> a 5)) '(1 2 3 4 5 10 12))

  ;; Example: assume you have student scores for multiple students

  
(define x (lambda (w) (+ w 2)))
(define y (lambda (a b) (* a b)))
(define grade-adjuster (lambda (boost-amt)
                         (lambda (grade) (+ grade boost-amt))))

(define multiply3 (lambda (a)
                    (lambda (b)
                      lambda (c)
                      (* a b c))))


(map (lambda (mylist)
     (fiter (lambda (s) (not(= 0s))) my list)))

 ;;recuding aka foldl and dold r
 ;; foldl = fold from the left
     ;; fold r = fold from the right
  ;; reducting or folding means collapse a list into a sigle result

    ;; example
adds up all the values
(foldl + 0.0 '(1 3 4 2))
what happens
0.0 + 1 
1.0 + 3
4.0 + 4 8
 8.0 + 2 = 10
the rule is foldl needs a fucntion (like +) that accepts
two imputs: first input needs to be folder valuse so rar
the second input will be whetere is next in the list




     (foldl (lambda( max-so-far val-from-list)
              (if (> val-from list max-so-far)
                  val-from list
                  max-so-far))
            0
            '(1 2 3 4 5))

             ;;(( 80 30 70 70) (90 85 90 90) (30 10 0 5)

|#

(define (passing threshold)
  (lambda (grades)
   (length

  (require


 ;; ::how the top 10 peices ( collum5 in terms of occeurance in the data
    ;; show the % of rows for each of thouse speices
   ;; use fold map reduce

   ;; you can use sort 
   