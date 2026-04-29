#lang racket
(require "env.rkt")
(provide initial-env)

(define (lisp-apply f args) (apply f args))
(define (lisp-list . xs) xs)

(define built-in-xs '(+ - * / car cdr cons < > not apply null? pair? number? symbol? list))
(define built-in-vs (list + - * / car cdr cons < > not lisp-apply null? pair? number? symbol? lisp-list))

(define initial-env
  (extend-env*
    empty-env
    built-in-xs
    built-in-vs))
