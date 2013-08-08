;; TODO
;; - add 'startOfLine to dispatch
;; - add 'endOfLine to dispatch
;; - add 'reset to dispatch
;; - add 'then       "
;; - add 'find       "
;; - add 'maybe      "

#lang racket
(require "./ve-strings.rkt")
(require "ve-expressions.rkt")

(module+ test
  (require test-engine/racket-tests))

(provide make-ve)
;; verbal-expressions.rkt
;; 130806 by Ben Rudgers
;; see 
;; https://github.com/jehna/VerbalExpressions
;; Expressions based VerbalExpressions.js
;; commit 41cd3f74a1025af43374a35c6b08499e64207696
;; 
;; Diferences:
;; - "or" renamed to "OR" to aviod naming issues
;; -  ^^^ kludge needs fixing
;;
;; Uses message passing model
;; Internal use of set! only

;; String -> String
;; sanitation for safely adding
;; anything to string
;; !!!
(define (ve-sanitize str) str) ; stub





;; Symbol -> Side Effect | ""
(define (sym-dispatch sym)
  (cond [(eq? sym 'anything)(anything)]
        [(eq? sym 'somthing)(something)]
        [(eq? sym 'lineBreak)(lineBreak)]
        [(eq? sym 'br)(br)]
        [(eq? sym 'tab)(tab)]
        [(eq? sym 'word)(word)]
        [else "single"]))


;; Symbol String -> Side Effect | String
(define (sym-str-dispatch sym str)
  (cond [(eq? sym 'anythingBut)(anythingBut  str)]
        [(eq? sym 'somethingBut)(somethingBut  str)]
        [(eq? sym 'anyOf)(anyOf str)]
        [(eq? sym 'any)(any str)]
        [else "double"]))


;; Symbol String String -> Side Effect | ""
(define (sym-str-str-dispatch sym s1 s2)
  (cond [(eq? sym 'replace)(replace s1 s2)]
        [else "triple"]))


;; List -> Side Effect | ""
(define (fn-for-fn fn)
  (cond [(empty? fn) (regex)]
        [(empty? (rest fn))
         (sym-dispatch (first fn))]
        [(empty? (cddr fn))
         (sym-str-dispatch (first fn)
                           (second fn))]
        [(empty? (cdddr fn))
         (sym-str-str-dispatch (first fn)
                               (second fn)
                               (third fn))]
        [else "fn-for-fn"]))


;; (listof X) -> String
(define (fn-for-lox lox)
  (cond [(empty? lox) (regex)]
        [(empty? (rest lox)) ;; catch single last item
         (begin
           (fn-for-fn (first lox))
           (regex))]       
        [else
         (string-append (fn-for-fn (first lox))
                        (fn-for-lox (rest lox)))]))

(define (make-ve)     
  fn-for-lox)

(module+ test
  (test))


