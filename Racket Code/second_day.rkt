#lang racket
;; dictionaly operations
;; e.g (a 1)
;; what functions that can do: d get
;; we are using a reducation system
;; ( + 3 5) reduces to 8

   ; define and d get is the function

(define (dget d key not-found)
  (cond ((empty? d) not-found)
        ((equal? key (first (first d)))
         (second (first d)))
        (#t (dget (rest d) key not-found))))

(define dtmp '((a 1) (b 2) (c 3)))
(dget dtmp 'a -1)
(dget dtmp 'b -1)
(dget dtmp 'c -1)
(dget dtmp 'd -1)

;; technique: for every pair that;s not related to the key,
;; just cons it as it is; when you encounter the key
;;cons the new value instead; if you neve encountered the key
;;just cons the new key/ value at the end;
;;in other words, construct an entirely new list when the data
;;in the form you want it 

(define (dset d key val)
  (cond ((empty? d)(list(list key val)))
        ((equal? key (first (first d))))
         (cons (list key val)
         (rest d)))
  (#t (cons (first d)
            (dset(rest d) key val))))

#|
 (define (func1 nums value)
   (cond (> nums value)
         (rest nums)))

  (define (func2 nums value)
   (cond ((empty? list) 0)
    (( + ( nums) (=value value)))))
 |#

;;given a number and a target does any value in my list add to target

(define (func2 a nums target)
  (println (list a nums target))
  (if ( empty? nums) -1
        (if (equal? target (+ a (first nums)))
            (list a (first nums))
            
             (func2 a (rest nums) target))))

(define (fun1 lst target)
  if(empty> lst) -1
  (let ((val 1 (first lst))
        val 2 (fun2 (first lst)(rest lst)(target)))




 ;; challange knock out some leetcode add two numbers the reverse number 2
 ;; compute the average ofa list of number
 ;; find the most common value in a list probs use d set / d get
 ;; difficult, but might be intresting: calculus derivatice function


             
        
  
         