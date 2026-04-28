#lang racket
(require "reader.rkt" "eval.rkt" "built-ins.rkt")
(provide repl)

(define (pretty-print v)
  (cond
    ;; number
    [(number? v) (number->string v)]
    ;; symbol
    [(symbol? v) (symbol->string v)]
    ;; quote
    [(and (pair? v)
          (eq? (car v) 'quote)
          (= (length v) 2))
     (string-append "'" (pretty-print (cadr v)))]
    ;; list
    [(list? v)
     (string-append
       "("
       (string-join (map pretty-print v) " ")
       ")")]
    ;; closure
    [(and (pair? v) (eq? (car v) 'Closure))
     "<closure>"]
    [else (format "~a" v)]))

(define (repl)
  (displayln "OI!")
  (displayln "Type :quit to exit.")
  (let loop ([env initial-env])
    (display "λ> ")
    (flush-output)
    (define input (read-line))
    (cond
      [(eof-object? input) (displayln "\n1551")]
      [(string=? input "") (loop env)]
      [(string=? input ":quit") (displayln "1551")]
      [else
       (with-handlers ([exn:fail?
                        (lambda (e)
                          (displayln (format "Error: ~a" (exn-message e)))
                          (loop env))])
         (let*
           ([ast (read-expr input)]
            [eval-res (eval-expr ast env)]
            [value (car eval-res)]
            [new-env (cdr eval-res)])
           (displayln (string-append "OI: " (pretty-print value)))
           (loop new-env)))])))

(module+ main (repl))
