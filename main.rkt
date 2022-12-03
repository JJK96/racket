#lang racket
(require (rename-in racket/base (+ base:+) (= base:=) (- base:-)))
(require racket/match)

(provide + - = ref)

(define (from-char x)
  (cond
     [(char? x) (char->integer x)]
     [else x]))

(define (+ . args)
  (cond [(andmap string? args) (apply string-append args)]
        [(ormap char? args) (integer->char (apply base:+ (map from-char args)))]
        [else (apply base:+ args)]))

(define (- . args)
  (cond [(ormap char? args) (integer->char (apply base:- (map from-char args)))]
        [else (apply base:- args)]))

(define (= . args)
  (cond [(andmap string? args) (apply string=? args)]
        [(andmap char? args) (apply char=? args)]
        [else (apply base:= args)]))

(define-syntax-rule (ref container . args)
  (let 
    ([func (cond [(list? container) list-ref]
                 [(hash? container) hash-ref]
                 [(vector? container) vector-ref]
                 [(string? container) string-ref])])
    (func container . args)))
     
(define mylist (list 1 2 3))
(define mymap (make-hash '[("a" . 1) ("b" . 2) ("c" . 3)]))
(define myvector #(1 2 3))
