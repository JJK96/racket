#lang racket
(require (rename-in racket/class (new base:new) (object% base:object%)))
(require racket/match)
(require racket/generator)
(require (for-syntax racket/syntax))
(require syntax/parse (for-syntax syntax/parse))
(require
    threading
    data/collection)
(require (rename-in (only-in racket/base + = - substring) 
             (+ base:+) (= base:=) (- base:-) (substring base:substring)))

(provide + - = 
  char->number 
  substring
  with-input
  object%
  new
  in-string
  generator
  for/match
  for/list/match
  lambda/match
  bytes->string/utf-16
  yield
  (all-from-out threading)
  (all-from-out data/collection))

(define-syntax-rule (substring . args)
  (with-handlers ([exn:fail:contract? (lambda (e) "")])
    (base:substring . args)))

(define (from-char x)
  (cond
     [(char? x) (char->integer x)]
     [else x]))

(define (char->number char)
  (- (char->integer char) 48))

(define (+ . args)
  (cond [(andmap string? args) (apply string-append args)]
        [(ormap char? args) (apply base:+ (map from-char args))]
        [else (apply base:+ args)]))

(define (- . args)
  (cond [(ormap char? args) (apply base:- (map from-char args))]
        [else (apply base:- args)]))

(define (= . args)
  (cond [(andmap string? args) (apply string=? args)]
        [(andmap char? args) (apply char=? args)]
        [(andmap symbol? args) (apply symbol=? args)]
        [else (apply base:= args)]))

(define-syntax-rule (with-input filename . rest)
    (with-input-from-file filename
      (lambda () . rest))) 

(define object%
  (class* base:object% (printable<%>)
     (super-new)
     (init-field [_classname "object%"])
     (define/public (custom-write out)
        (fprintf out "(")
        (fprintf out (get-field _classname this))
        (fprintf out " ")
        (fprintf out (string-join
                       (for/list ([field (field-names this)]
                                  #:when (not (= field '_classname)))
                         (+ 
                            (symbol->string field)
                            "="
                            (~v (dynamic-get-field field this))))))
        (fprintf out ")"))
     (define/public (custom-display out)
        (custom-write out))
     (define/public (custom-print out depth)
        (custom-write out))))


(define-syntax-rule (new myclass . rest)
  (let ([classname (~v myclass)])
    (base:new myclass [_classname classname] . rest)))

(define (in-string str)
  (in-list (string->list str)))

(define-syntax (generator stx)
    (define-syntax-class maybe-arity
      (pattern (#:arity arity)))
    (syntax-parse stx
        [(_ (~optional maybe-arity) body ...)
         #'(sequence->stream (in-generator (~? maybe-arity) body ...))]))

(begin-for-syntax
  (define (generic-for/match stx for-func)
    (define-splicing-syntax-class maybe-break
      (pattern (~seq #:break break)
               #:with res #'(#:break break))
      (pattern (~seq #:final final)
               #:with res #'(#:final final))
      (pattern (~seq)
               #:with res #'()))
    (syntax-parse stx
        [(_ ([pat seq-expr]) maybe-break:maybe-break body ...)
         #:with x (generate-temporary)
         #`(#,for-func ([x seq-expr])
            (~@ . maybe-break.res)
            (match x
              [pat body ...]))])))

(define-syntax (for/match stx)
  (generic-for/match stx #'for))

(define-syntax (for/list/match stx)
  (generic-for/match stx #'for/list))

(define-syntax (lambda/match stx)
  (syntax-parse stx
    [(_ pat body ...)
     #:with x (generate-temporary)
     #'(lambda (x)
         (match x
           [pat body ...]))]))

(define (bytes->string/utf-16 bytes)
    (define converter (bytes-open-converter "UTF-16le" "UTF-8"))
    (let-values ([(bytes1 x y) (bytes-convert converter bytes)]) 
         (bytes->string/utf-8 bytes1)))
  
(define mylist (list 1 2 3))
(define mymap (make-hash '[("a" . 1) ("b" . 2) ("c" . 3)]))
(define myvector #(1 2 3))
