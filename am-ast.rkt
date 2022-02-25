#lang racket

; ==========================================
; The Abstract Machine Commands and Programs
; ==========================================

(provide
 (prefix-out
  am:
  (combine-out
   (except-out (all-defined-out)
               check? error:get am:and
               one-argument? two-arguments?)
   (rename-out [am:and and]))))

(require "sequence.rkt")
  
; =========================================
; AM Commands 
; =========================================

; Design principles for the implementation
; of commands:
; - all commands are either an integer
;   or a pair starting with an integer
; - when the command is a pair, the
;   second component is either:
;   * an integer
;   * a symbol representing a variable
;   * a pair of programs

; ---------------------------
; Constructors
; ---------------------------

; The constructors have three forms:
; - either they are constants:
;   add, sub, mul, true, false, and,
;   lt, eq, not, print, input
; - or they are procedures taking
;   one argument either a number:
;     push
;   or a symbol representing a variable:
;     fetch, store
; - or they are procedures taking
;   two AM programs as arguments:
;   branch, loop.

; Arithmetic Commands
(define add 0)
(define sub 1)
(define mul 2)
(define (push integer)
  (cons 3 integer))
(define (fetch symbol)
  (cons 4 symbol))

; Boolean Commands
(define true 10)
(define false 11)
(define am:and 12)
(define not 13)
(define eq 14)
(define lt 15)

; Commands
(define (store symbol)
  (cons 20 symbol))
(define input 21)
(define print 22)
(define (branch then else)
  (cons 23 (cons then else)))
(define (loop condition body)
  (cons 24 (cons condition body)))

; ---------------------------
; Predicates
; ---------------------------

; Procedure: index
; Argument: an AM command
; Return: an integer uniquely
;  characterizing the AM command
(define (index com)
  (cond [(integer? com) com]
        [(pair? com) (car com)]
        [else 'Undefined]))

; Procedure: check?
; Argument: ref-com, an AM command
; Return: a predicate that
;   checks if its argument
;   is similar to the
;   reference command 
(define (check? ref-com)
  (let ((int (index ref-com)))
    (lambda (com) (= (index com) int))))

; All the following predicates
; take as argument one AM command
; and return #t if and only if
; the command is the expected command

(define add? (check? add))
(define sub? (check? sub))
(define mul? (check? mul))
(define push? (check? (push 0)))
(define fetch? (check? (fetch 'x)))
(define true? (check? true))
(define false? (check? false))
(define and? (check? am:and))
(define not? (check? not))
(define eq? (check? eq))
(define lt? (check? lt))
(define store? (check? (store 'x)))
(define input? (check? input))
(define print? (check? print))
(define branch? (check? (branch null null)))
(define loop? (check? (loop null null)))

; ---------------------------
; Accessor / Selector
; ---------------------------

(define (one-argument? command)
  (or (fetch? command)
      (store? command)
      (push? command)))

(define (two-arguments? command)
  (or (branch? command)
      (loop? command)))

(define error:get
  (string-append
   "(am:get com) expects com to "
   "satisfy fetch?, store? or push?.\n"
   "(am:get com index) expects com to "
   "satisfy branch? or loop? and "
   "index to be 0 or 1."))

; Procedure: get
; Arguments:
; - command, an AM command satisfying:
;   fetch?, store?, push?, loop? or branch?
; - optionnally: index, 0 or 1
; Return: the argument to the AM command:
; - for a fetch or store command, a symbol
; - for a push command, an integer
; - for a branch or loop command, the first
;   or second program
(define get
  (lambda (command . index)
  (cond
    [(and (null? index) (one-argument? command))
      (cdr command)]
    [(and (= 1 (length index))(two-arguments? command))
     (cond [(= (car index) 0) (cadr command)]
           [(= (car index) 1) (cddr command)]
           [else (raise error:get)])]
    [else (raise error:get)])))

; =========================================
; AM Programs 
; =========================================

; AM Programs are sequences of AM commands

