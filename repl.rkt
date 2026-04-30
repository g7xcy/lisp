#lang racket
(require "reader.rkt" "eval.rkt" "built-ins.rkt" readline/readline)
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
    ;; closure
    [(and (pair? v) (eq? (car v) 'Closure))
     "<closure>"]
    ;; list
    [(list? v)
     (string-append
       "("
       (string-join (map pretty-print v) " ")
       ")")]
    [else (format "~a" v)]))

(define (balance-state s)
  (let loop ([chars (string->list s)] [n 0])
    (cond
      [(< n 0) 'error]
      [(null? chars)
       (cond
         [(= n 0) 'ok]
         [else 'incomplete])]
      [(char=? (car chars) #\()
       (loop (cdr chars) (add1 n))]
      [(char=? (car chars) #\))
       (loop (cdr chars) (sub1 n))]
      [else
       (loop (cdr chars) n)])))

(define (read-multiline)
  (let loop ([acc ""])
    (define line (readline (if (string=? acc "") "λ> " "... ")))
    (cond
      [(eof-object? line) line]
      [else
       (define new-acc
         (if (string=? acc "")
             line
             (string-append acc "\n" line)))
       (case (balance-state new-acc)
         [(ok) new-acc]
         [(incomplete) (loop new-acc)]
         [(error)
          (displayln "Parse Error: unexpected ')'")
          (loop "")])])))

(define (repl)
  (displayln "OI!")
  (displayln "Type :quit to exit.")
  (let loop ([env initial-env])
    (define input (read-multiline))
    (cond
      [(eof-object? input) (displayln "\noiiai")]
      [(string=? input "") (loop env)]
      [(string=? input ":quit") (displayln "oiiai")]
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
