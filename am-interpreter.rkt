#lang racket

(provide am:exec am:run)

(require "am-ast.rkt"
         "environment.rkt"
         "configuration.rkt"
         "sequence.rkt"
         "stack.rkt")

;Used to get index of various operations
(define value -99)

;Execute a sequence of am commands with a stack and environment
(define am:exec
  (lambda (cfg)
    (if (seq:empty? (cfg:code cfg))
        cfg
        (let ((firstCom (seq:head (cfg:code cfg)))
              (stack (cfg:stack cfg))
              (env (cfg:env cfg)))
          (cond
          ;Constants
          ((number? firstCom) (cond
                                ;Push 1st value + 2nd value on stack
                                ((= firstCom am:add) (am:exec
                                                      (cfg:make (seq:tail (cfg:code cfg))
                                                                (stack:push (+ (stack:top stack) (stack:top (stack:pop stack))) (stack:pop (stack:pop stack)))
                                                                env)))
                                ;Push 1st value - 2nd value on stack
                                ((= firstCom am:sub) (am:exec
                                                      (cfg:make (seq:tail (cfg:code cfg))
                                                                (stack:push (- (stack:top stack) (stack:top (stack:pop stack))) (stack:pop (stack:pop stack)))
                                                                env)))
                                ;Push 1st value * 2nd value on stack
                                ((= firstCom am:mul) (am:exec
                                                      (cfg:make (seq:tail (cfg:code cfg))
                                                                (stack:push (* (stack:top stack) (stack:top (stack:pop stack))) (stack:pop (stack:pop stack)))
                                                                env)))
                                ;Push negation of top boolean on stack
                                ((= firstCom am:not) (am:exec
                                                      (cfg:make (seq:tail (cfg:code cfg))
                                                                (stack:push (if (= (stack:top stack) am:true)
                                                                                am:false
                                                                                am:true)
                                                                            (stack:pop stack))
                                                                 env)))
                                ;Push boolean conjunction of top two stack values
                                ((= firstCom am:and) (am:exec
                                                      (cfg:make (seq:tail (cfg:code cfg))
                                                                (stack:push (if (and (= (stack:top stack) am:true) (= (stack:top (stack:pop stack)) am:true))
                                                                                am:true
                                                                                am:false)
                                                                            (stack:pop (stack:pop stack)))
                                                                env)))
                                ;Test if top value on stack < second value on stack
                                ((= firstCom am:lt) (am:exec
                                                      (cfg:make (seq:tail (cfg:code cfg))
                                                                (stack:push (if (< (stack:top stack) (stack:top (stack:pop stack)))
                                                                                am:true
                                                                                am:false)
                                                                            (stack:pop (stack:pop stack)))
                                                                env)))
                                 ;Test if top two values on stack are equal
                                 ((= firstCom am:eq) (am:exec
                                                      (cfg:make (seq:tail (cfg:code cfg))
                                                                (stack:push (if (= (stack:top stack) (stack:top (stack:pop stack)))
                                                                                am:true
                                                                                am:false)
                                                                            (stack:pop (stack:pop stack)))
                                                                env)))
                                 ;Print Operation
                                 ((= firstCom am:print) (displayln (stack:top stack))
                                                        (am:exec
                                                         (cfg:make (seq:tail (cfg:code cfg))
                                                                   (stack:pop stack)
                                                                   env)))
                                 ;Input Operation
                                 ((= firstCom am:input) (am:exec
                                                         (cfg:make (seq:tail (cfg:code cfg))
                                                                   (stack:push (read) stack)
                                                                   env)))))
                                 
          
          ;Single Arguments
          ;Push a value onto the stack
          ((= (am:index (am:push value)) (am:index firstCom)) (am:exec
                                                               (cfg:make (seq:tail (cfg:code cfg))
                                                                         (stack:push (am:get firstCom) stack)
                                                                         env)))
          ;Fetch variable's values and push them onto the stack onto the stack
          ((= (am:index (am:fetch value)) (am:index firstCom)) (am:exec
                                                                (cfg:make (seq:tail (cfg:code cfg))
                                                                          (stack:push (env:get (cfg:env cfg) (am:get firstCom)) stack)
                                                                          env)))
          ;Store the stacks top values in the variable
          ((= (am:index (am:store value)) (am:index firstCom)) (am:exec
                                                                (cfg:make (seq:tail (cfg:code cfg))
                                                                          (stack:pop stack)
                                                                          (env:update env (am:get firstCom) (stack:top stack)))))
          ;Double Arguments
          ;Branch
          ((= (am:index (am:branch seq:empty seq:empty)) (am:index firstCom)) (if (= (stack:top stack) am:true)
                                                                                  (am:exec
                                                                                   (cfg:make (seq:concat (am:get firstCom 0) (seq:tail (cfg:code cfg)))
                                                                                             (stack:pop stack)
                                                                                             env))
                                                                                  (am:exec
                                                                                   (cfg:make (seq:concat (am:get firstCom 1) (seq:tail (cfg:code cfg)))
                                                                                             (stack:pop stack)
                                                                                             env))))
          ;Loop
          ((= (am:index (am:loop am:true seq:empty)) (am:index firstCom)) (let ((result (stack:top (cfg:stack (am:exec (cfg:make (am:get firstCom 0) seq:empty env))))))
                                                                           (if (= result am:true)
                                                                                (am:exec
                                                                                 (cfg:make (seq:concat (am:get firstCom 1)
                                                                                                       (seq:single (am:loop 
                                                                                                                    (am:get firstCom 0)
                                                                                                                    (am:get firstCom 1)))
                                                                                                       (seq:tail (cfg:code cfg)))
                                                                                           stack
                                                                                           env))
                                                                                (am:exec
                                                                                 (cfg:make (seq:tail (cfg:code cfg))
                                                                                           stack
                                                                                           env))))))))))

;Executes a program starting from a completely empty configuration
(define am:run
  (lambda (prog)
    (am:exec (cfg:make prog stack:empty env:empty))))
