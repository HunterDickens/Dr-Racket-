#lang racket

#| how oop works

-- need the abiltiy to creatn an "oobject that has certain popertiys(data)
and this object keeps track of those properties; they can change as ell
- two different objects of the same "class" template would have the same
names for their properties but not the same values
-- also want "methods to be avaiable that manipulate the object.

key idea: an oject is essentially a dictiionary ; the property names are key and
the property valuse are the values of the dictiionay

typically, the dictionaly know what time of class it is e.g car.
this can be accomplished by just add antoher key to the dictionaly

when creating a method for a class, it expects to be able access the property
dictionarly to either set values or get values. so each method for the class
needs to have some kind of "self" thing that is literally just a dictionaly 


|#
(define Car
  (list '(model "none" 'speed 0)
        (list (list 'increase-speed
                   
                    ;(lambda (self) (dset self 'speed (+1 (dget self 'speed))))
                    (lambda (self
                    (list 'get-speed
                          (lambda (self) (dget self 'speed)))))))

 (define (obj-new class-template)
   (map (lambda (p) (list p null))
        (first class-template)))
                            
(define corvett)
