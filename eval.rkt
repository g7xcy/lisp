#lang racket
(require "env.rkt")
(provide eval-expr)

(define (quote->value ast)
  (match ast
    [`(Number ,n) n]
    [`(Symbol ,x) x]
    [`(List ,xs ...) (map quote->value xs)]
    [`(quote ,x) (list 'quote (quote->value x))]
    [_ ast]))

;; eval :: Expr -> Env -> (Value Env)
(define (eval-expr ast env)
  (match ast
    ;; Number
    [`(Number ,n) (cons n env)]
    ;; Symbol
    [`(Symbol ,x) (cons (lookup env x) env)]
    ;; Quote
    [`(quote ,x) (cons (quote->value x) env)]
    ;; if
    [`(List (Symbol if) ,condition ,then ,else)
     (let ([c (car (eval-expr condition env))])
       (if c
           (eval-expr then env)
           (eval-expr else env)))]
    ;; define expr
    [`(List (Symbol define) (Symbol ,x) ,expr)
     (let*
       ([res (eval-expr expr env)]
        [val (car res)]
        [new-env (extend-env env x val)])
       (cons val new-env))]
    ;; let expr
    [`(List (Symbol let) (List ,bindings ...) ,body)
     (let*
       ([xs
         (map (lambda (b)
                (match b
                  [`(List (Symbol ,x) ,_) x]))
              bindings)]
        [vs (map (lambda (b)
                   (match b
                     [`(List (Symbol ,_) ,expr) (car (eval-expr expr env))]))
                 bindings)]
        [new-env (extend-env* env xs vs)])
       (cons (car (eval-expr body new-env)) env))]
    ;; lambda expr
    [`(List (Symbol lambda) (List ,params ...) ,body)
     (let ([xs (map (lambda (p)
                      (match p
                        [`(Symbol ,x) x]))
                    params)])
       (cons `(Closure ,xs ,body ,env) env))]
    ;; function apply
    [`(List ,f ,args ...)
     (let* ([f-val (car (eval-expr f env))]
            [arg-vals (map (lambda (a) (car (eval-expr a env))) args)])
       (match f-val
         ;; bulit in functions
         [(? procedure?) (cons (apply f-val arg-vals) env)]
         ;; user defined functions
         [`(Closure ,params ,body ,closure-env)
          (unless (= (length params) (length arg-vals))
            (error "arity mismatch"))
          (let ([new-env (extend-env* closure-env params arg-vals)])
            (cons (car (eval-expr body new-env)) env))]
         ;; error: mismatch
         [_ (error "Not a function" f-val)]))]))
