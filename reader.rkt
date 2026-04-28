#lang racket
(provide tokenize read-expr)

;; Parser a -> string -> (cons a String) | #f
;; pure :: a -> Parser a
(define (pure a)
  (lambda (s) (cons a s)))
;; failure :: a -> Parser a
(define failure
  (const #f))
;; bind ::Parser a -> (a -> Parser b) -> Parser b
(define (bind m f)
  (lambda (s)
    (let ([res (m s)])
      (if (not res)
          #f
          ((f (car res)) (cdr res))))))
;; fmap :: (a -> b) -> Parser a -> Parser b
(define (fmap f m)
  (bind m
        (lambda (x)
          (pure (f x)))))
;; >> :: Parser a -> Parser a -> Parser a
(define (>> p q)
  (bind p
        (const q)))
;; << :: Parser a -> Parser a -> Parser a
(define (<< p q)
  (bind p
        (lambda (res)
          (bind q
                (const (pure res))))))
;; choice :: Parser a -> Parser b -> Parser a | Parser b
(define (choice p q)
  (lambda (s)
    (let ([res (p s)])
      (if (not res)
          (q s)
          res))))
;; choices :: [Parser a] -> Parser [a]
(define (choices ps)
  (foldr choice
         failure
         ps))
;; some :: Parser a -> Parser [a]
(define (some p)
  (bind p
        (lambda (res)
          (bind (many p)
                (lambda (rest)
                  (pure (cons res rest)))))))
;; many :: Parser a -> Parser [a]
(define (many p)
  (choice (some p) (pure '())))
(define (empty-input? input)
  (or (and (string? input) (string=? input ""))
      (and (list? input) (null? input))))
(define (run-parser p input)
  (let ([res (p input)])
    (cond
      [(not res) #f]
      [(empty-input? (cdr res)) (car res)]
      [else #f])))

;; Tokenize
;; Token : String
;; satisfy :: (Char -> Bool) -> Parser Char
(define (satisfy pred)
  (lambda (s)
    (if (and (> (string-length s) 0)
             (pred (string-ref s 0)))
        (cons (string-ref s 0)
              (substring s 1))
        #f)))
(define tokenize-lparen
  (fmap (const "(")
        (satisfy (curry char=? #\())))
(define tokenize-rparen
  (fmap (const ")")
        (satisfy (curry char=? #\)))))
(define tokenize-quote
  (fmap (const "'")
        (satisfy (curry char=? #\'))))
(define tokenize-number
  (fmap list->string
        (some (satisfy char-numeric?))))
(define (char-symbol? c)
  (and (not (char-whitespace? c))
       (not (char=? c #\())
       (not (char=? c #\)))
       (not (char=? c #\'))))
(define tokenize-symbol
  (fmap list->string
        (some (satisfy char-symbol?))))
(define tokenize-whitespace
  (fmap (const #f)
        (some (satisfy char-whitespace?))))
(define parse-token
  (choices
    (list
      tokenize-whitespace
      tokenize-lparen
      tokenize-rparen
      tokenize-quote
      tokenize-number
      tokenize-symbol)))
(define tokenize-parser
  (fmap (lambda (tokens) (filter identity tokens))
        (many parse-token)))
(define (tokenize input)
  (run-parser tokenize-parser input))

#|
(X) Expr       -> Atom | List | Quote
(X) Atom       -> Number | Symbol
(X) List       -> ( Elements )
(X) Elements   -> Expr Elements | ε
(X) Quote      -> ' Expr

(X) Digit      -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
(X) Number     -> Digit NumberRest
(X) NumberRest -> Digit NumberRest | ε
(X) Char       -> ASCII (excluding whitespace, (, ), ')
(x) Symbol     -> Char Symbol | Char
|#

;; Parser :: a -> (cons a [Token]) | #f
;; satisfy-token :: (String -> Bool) -> Parser String
(define (satisfy-token pred)
  (lambda (tokens)
    (if (and (not (null? tokens))
             (pred (car tokens)))
        (cons (car tokens) (cdr tokens))
        #f)))
(define (numeric-string? s)
  (andmap char-numeric? (string->list s)))
(define (string->ast-number s)
  `(Number ,(string->number s)))
(define parse-number
  (fmap string->ast-number
        (satisfy-token numeric-string?)))
(define (accepted-char? c)
  (and (not (char-whitespace? c))
       (not (char=? #\( c))
       (not (char=? #\) c))
       (not (char=? #\' c))))
(define (symbol-string? s)
  (and (not (string->number s))
       (andmap accepted-char? (string->list s))))
(define (string->ast-symbol s)
  `(Symbol ,(string->symbol s)))
(define parse-symbol
  (fmap string->ast-symbol
        (satisfy-token symbol-string?)))
(define parse-atom
  (choice parse-number parse-symbol))
(define parse-lparen
  (satisfy-token (curry string=? "(")))
(define parse-rparen
  (satisfy-token (curry string=? ")")))
(define parse-expr
  (letrec ([parse-expr (lambda (tokens) ((choices (list parse-quote parse-list parse-atom)) tokens))]
           [parse-elements (many parse-expr)]
           [parse-list
            (fmap (lambda (tokens) `(List ,@tokens))
                  (>> parse-lparen
                      (<< parse-elements parse-rparen)))]
           [parse-quote
            (fmap (lambda (token) `(quote ,token))
                  (>> (satisfy-token (curry string=? "'"))
                      parse-expr))])
    parse-expr))
(define (read-expr input)
  (let ([tokens (tokenize input)])
    (and tokens
         (run-parser parse-expr tokens))))
