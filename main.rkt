#lang racket
(require (rename-in racket/base (+ base:+) (= base:=) (- base:-) (substring base:substring)))
(require (rename-in racket/class (new base:new) (object% base:object%)))
(require racket/match)
(require racket/generator)

(provide + - = 
  char->number 
  substring
  with-input
  object%
  new
  in-string
  generator
  for/match)

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

(define-syntax-rule (ref container . args)
  (let 
    ([func (cond [(list? container) list-ref]
                 [(hash? container) hash-ref]
                 [(vector? container) vector-ref]
                 [(string? container) string-ref])])
    (func container . args)))

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
    (syntax-parse stx
        [(_ body ...)
         #'(sequence->stream (in-generator body ...))]))

(define-syntax (for/match stx)
    (syntax-parse stx
        [(_ ([pat seq-expr]) body ...)
         #:with x (generate-temporary)
         #'(for ([x seq-expr])
              (match x
               [pat body ...]))]))


(define mylist (list 1 2 3))
(define mymap (make-hash '[("a" . 1) ("b" . 2) ("c" . 3)]))
(define myvector #(1 2 3))
