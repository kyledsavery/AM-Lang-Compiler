#lang racket

(provide w1:compiler)

(require "w1-ast.rkt"
         "am-ast.rkt"
         "sequence.rkt")

(define w1:compiler
  (lambda (commands)
    (if (seq:empty? commands)
        seq:empty
        (let ((firstCom (seq:head commands)))
             (cond
               ;Assign Operation
               ((w1:is? firstCom 'assign) (seq:concat (eval:expr (w1:get:exp firstCom)) (seq:single (am:store (w1:get:var firstCom))) (w1:compiler (seq:tail commands))))
               ;If Operation
               ((w1:is? firstCom 'if) (cond
                                        ;Check for true
                                        ((w1:is? (w1:get:exp firstCom) 'true) (seq:concat (seq:single am:true)
                                                                                            (seq:single (am:branch (w1:compiler (w1:get:block firstCom 'then))
                                                                                                                   (w1:compiler (w1:get:block firstCom 'else))))
                                                                                            (w1:compiler (seq:tail commands))))
                                        ;Check for false
                                        ((w1:is? (w1:get:exp firstCom) 'false) (seq:concat (seq:single am:false)
                                                                                             (seq:single (am:branch (w1:compiler (w1:get:block firstCom 'then))
                                                                                                                    (w1:compiler (w1:get:block firstCom 'else))))
                                                                                             (w1:compiler (seq:tail commands))))
                                        ;Check for equals
                                        ((w1:is? (w1:get:exp firstCom) 'eq)(seq:concat (eval:expr (w1:get:exp (w1:get:exp firstCom) 'right))
                                                                                       (eval:expr (w1:get:exp (w1:get:exp firstCom) 'left))
                                                                                       (seq:single am:eq)
                                                                                       (seq:single (am:branch (w1:compiler (w1:get:block firstCom 'then))
                                                                                                              (w1:compiler (w1:get:block firstCom 'else))))
                                                                                       (w1:compiler (seq:tail commands))))
                                        ;Check for less than
                                        ((w1:is? (w1:get:exp firstCom) 'lt)(seq:concat (eval:expr (w1:get:exp (w1:get:exp firstCom) 'right))
                                                                                       (eval:expr (w1:get:exp (w1:get:exp firstCom) 'left))
                                                                                       (seq:single am:lt)
                                                                                       (seq:single (am:branch (w1:compiler (w1:get:block firstCom 'then))
                                                                                                              (w1:compiler (w1:get:block firstCom 'else))))
                                                                                       (w1:compiler (seq:tail commands))))
                                        ;Check for negation
                                        ((w1:is? (w1:get:exp firstCom) 'not)(seq:concat (eval:expr (w1:get:exp (w1:get:exp firstCom)))
                                                                                        (seq:single am:not)
                                                                                        (seq:single (am:branch (w1:compiler (w1:get:block firstCom 'then))
                                                                                                               (w1:compiler (w1:get:block firstCom 'else))))
                                                                                        (w1:compiler (seq:tail commands))))
                                        ;Check for conjunction
                                        (else (seq:concat (eval:expr (w1:get:exp (w1:get:exp firstCom) 'right))
                                                                                        (eval:expr (w1:get:exp (w1:get:exp firstCom) 'left))
                                                                                        (seq:single am:and)
                                                                                        (seq:single (am:branch (w1:compiler (w1:get:block firstCom 'then))
                                                                                                               (w1:compiler (w1:get:block firstCom 'else))))
                                                                                        (w1:compiler (seq:tail commands))))))
               ;Print Operation
               ((w1:is? firstCom 'print) (seq:concat (eval:expr (w1:get:exp firstCom)) (seq:single am:print) (w1:compiler (seq:tail commands))))
               ;Input Operation
               ((w1:is? firstCom 'input) (seq:concat (seq:single am:input) (seq:single (am:store (w1:get:var firstCom))) (w1:compiler (seq:tail commands))))
               ;Loop Operation
               (else (seq:make (am:loop (cond
                                          ((w1:is? (w1:get:exp firstCom) 'true) (seq:single am:true))
                                          ((w1:is? (w1:get:exp firstCom) 'false) (seq:single am:false))
                                          (else (eval:expr (w1:get:exp firstCom))))
                                        (w1:compiler (w1:get:block firstCom)))
                               (w1:compiler (seq:tail commands)))))))))


;Turns arithmetic and boolean expressions to am code
(define eval:expr
  (lambda (expr)
   (cond
     ((number? expr) (seq:single (am:push expr)))
     ((symbol? expr) (seq:single (am:fetch expr)))
     ((w1:is? expr 'add) (seq:concat (eval:expr (w1:get:exp expr 'right)) (eval:expr (w1:get:exp expr 'left)) (seq:single am:add)))
     ((w1:is? expr 'mul) (seq:concat (eval:expr (w1:get:exp expr 'right)) (eval:expr (w1:get:exp expr 'left)) (seq:single am:mul)))
     ((w1:is? expr 'sub) (seq:concat (eval:expr (w1:get:exp expr 'right)) (eval:expr (w1:get:exp expr 'left)) (seq:single am:sub)))
     ((w1:is? expr 'lt) (seq:concat (eval:expr (w1:get:exp expr 'right)) (eval:expr (w1:get:exp expr 'left)) (seq:single am:lt))))))


