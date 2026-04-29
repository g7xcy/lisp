#lang racket
(provide empty-env extend-env extend-env/ref extend-env* lookup)

;; Env :: Symbol -> (box Value)
;; empty-env :: Symbol -> (box Value)
(define empty-env
  (lambda (x)
    (error
      (format "Unbound variable: ~a" x))))

;; lookup :: Env -> Symbol -> (box Value)
(define (lookup env x)
  (unbox (env x)))

;; extend-env :: Env -> Symbol -> Value -> Env
(define (extend-env env x v)
  (lambda (y)
    (if (eq? x y)
        (box v)
        (env y))))

(define (extend-env/ref env x bx)
  (lambda (y)
    (if (eq? x y)
        bx
        (env y))))

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
