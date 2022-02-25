#lang racket

(provide seq:empty seq:make
         seq:empty? seq:head
         seq:tail seq:concat
         seq:single)

;Defines the empty sequence
(define seq:empty null)

;Creates a sequence with element as the first value
(define seq:make
  (lambda (element sequence)
     (cons element sequence)))

;Tests if a given sequence is empty
(define seq:empty?
  (lambda (sequence)
    (null? sequence)))

;Returns the first element of a nonempty sequence
(define seq:head
  (lambda (sequence)
    (car sequence)))

;Returns the tail of a sequence
(define seq:tail
  (lambda (sequence)
    (cdr sequence)))

;Concatenates all values into a sequence
(define (seq:concat . values)
  (concat:aux values))
    
;Helps seq:concat combine n number of sequence into single sequence
(define concat:aux
    (lambda (lst)
      (cond
        ((seq:empty? lst) seq:empty)
        ((seq:empty? (car lst)) (concat:aux (cdr lst)))
        (else (cons (caar lst) (concat:aux (cons (cdar lst) (cdr lst))))))))
   
;Returns the singleton sequence containing elt
(define seq:single
  (lambda (elt)
    (list elt)))

