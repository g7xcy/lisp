#lang racket
(provide empty-env extend-env extend-env* lookup)

;; Env :: Symbol -> Value
;; empty-env :: Symbol -> Value
(define empty-env
  (lambda (x)
    (error
      (format "Unbound variable: ~a" x))))

;; lookup :: Env -> Symbol -> Value
(define (lookup env x)
  (env x))

;; extend-env :: Env -> Symbol -> Value -> Env
(define (extend-env env x v)
  (lambda (y)
    (if (eq? x y)
        v
        (lookup env y))))

;; extend-env* :: Env -> [Symbol] -> [Value] -> Env
(define (extend-env* env xs vs)
  (cond
    [(and (null? xs) (null? vs)) env]
    [(or (null? xs) (null? vs))
     (error "extend-env*: length mismatch")]
    [else
     (extend-env*
       (extend-env env (car xs) (car vs))
       (cdr xs)
       (cdr vs))]))
