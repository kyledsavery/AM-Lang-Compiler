#lang racket

(provide (all-defined-out))

;Builds a configuration
(define cfg:make
  (lambda (e1 e2 e3)
    (list e1 e2 e3)))


;Returns the first component of a configuration
(define cfg:code
  (lambda (cfg)
    (car cfg)))

;Returns the second component
(define cfg:stack
  (lambda (cfg)
    (cadr cfg)))

;Returns the third component
(define cfg:env
  (lambda (cfg)
    (caddr cfg)))