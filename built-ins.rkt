#lang racket
(require "env.rkt")
(provide initial-env)

(define y-comb (lambda (fact) (lambda (x) (if (<= x 1) 1 (* x (fact (- x 1)))))))

(define initial-env
  (extend-env* empty-env
               '(+ - * / car cdr cons < > not y-comb)
               (list + - * / car cdr cons < > not y-comb)))
