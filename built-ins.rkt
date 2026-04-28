#lang racket
(require "env.rkt")
(provide initial-env)

(define initial-env
  (extend-env* empty-env
               '(+ - * / car cdr cons)
               (list + - * / car cdr cons)))
