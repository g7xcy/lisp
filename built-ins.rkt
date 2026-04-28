#lang racket
(require "env.rkt")
(provide initial-env)

(define initial-env
  (extend-env* empty-env
               '(+ - * /)
               (list + - * /)))
