#lang racket

(provide (all-defined-out))

;Constant for the empty stack
(define stack:empty null)

;Pushes a value on top of the stack
(define stack:push
  (lambda (new stack)
    (cons new stack)))

;Checks if the stack is empty
(define stack:empty?
  (lambda (stack)
    (null? stack)))

;Returns the top value of of the stack
(define stack:top
  (lambda (stack)
    (car stack)))

;Returns the stack with the top element popped
(define stack:pop
  (lambda (stack)
    (cdr stack)))