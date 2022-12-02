#lang racket
(require (rename-in racket/base (+ base:+)))
(require racket/match)

(provide + ref)

(define (+ . args)
  (cond [(andmap string? args) (apply string-append args)]
        [else (apply base:+ args)]))

(define-syntax-rule (ref container . args)
  (let 
    ([func (cond [(list? container) list-ref]
                 [(hash? container) hash-ref]
                 [(vector? container) vector-ref])])
    (func container . args)))
     
(define mylist (list 1 2 3))
(define mymap (make-hash '[("a" . 1) ("b" . 2) ("c" . 3)]))
(define myvector #(1 2 3))
