;; TODO
;; Allow Nested Replacement?
;; Implement Sanitize


;; ve-strings.rkt
;; contains core data structures
;; - the functions to directly set them
;; - (regex) to return the regex as a string

#lang racket
(module+ test
  (require test-engine/racket-tests))

(provide (all-defined-out))

;; Variables

(define ve-prefix "")
(define ve-source "")
(define ve-suffix "")
(define ve-modifiers "")
;; !!! needs full implementation
(define ve-replace empty) 

;; Functions

;; String -> String
;; sanitation for safely adding
;; anything to string
;;
;; JavaSript
;; // Sanitation function for adding
;; // anything safely to the expression
;; sanitize : function( value ) {
;;   if(value.source) 
;;     return value.source;
;;     return value.replace
;;        (/[^\w]/g, function(character) 
;;         {return "\\" + character; });
;; !!!
(define (ve-sanitize str) str) ; stub

;; -> SideEffects
;; reset all variables to zero
;; helpful for testing

(define (reset)
  (set! ve-prefix "")
  (set! ve-source "")
  (set! ve-suffix "")
  (set! ve-modifiers "")
  (set! ve-replace empty))


;; String -> String
;; prepends string to prefix portion of ve
(define(next-pre str)
  (set! ve-prefix
        (ve-sanitize 
         (string-append str ve-prefix))))



;; String -> String
;; appends string to src portion of ve
(define(next-src str)
  (set! ve-source 
        (ve-sanitize
         (string-append ve-source str))))


;; String -> SideEffect
;; appends string to mod portion of ve
(define(next-mod str)
  (let ((s (cond [(string=? "" ve-modifiers)
                  str]
                 [(regexp-match? str ve-modifiers)
                  ve-modifiers]
                 [else
                  (string-append ve-modifiers str)])))
    (set! ve-modifiers (ve-sanitize s))))

;; String -> SideEffect
;; appends string to suffix portion of ve
(define(next-suf str)
  (let ((s (cond [(string=? "" ve-suffix)
                  str]
                 [(regexp-match? str ve-suffix)
                  ve-suffix]
                 [else
                  (string-append ve-suffix str)])))
    (set! ve-suffix (ve-sanitize s))))

(module+ test
  ;; multi-unit test
  (check-expect (begin (reset)
                       (next-pre "^")
                       (next-src ";;")
                       (next-suf "$")
                       (next-mod "i")
                       (endCapture))
                (string-append "^"";;""$""i"))

  (check-expect (begin (reset)
                       (next-pre "^")
                       (next-src ";;")
                       (next-suf "$")(next-suf "$")
                       (next-mod "i")(next-mod "i")
                       (endCapture))
                (string-append "^"";;""$""i"))
    
  (check-expect (begin (reset)
                       (next-pre "^")
                       (next-src ";;")
                       (next-suf "$")(next-suf "k")
                       (next-mod "i")(next-mod "k")
                       (endCapture))
                (string-append "^"";;""$k""ik")))

;; String  -> String
;; replaces all occurances of 
;; does not handle nested replacement
;; ^^^^^^^^^^^^^^^^^ Kludge
(define (replace f r)
  (set! ve-replace (list f r)))

;; -> String
;; return the regular expression
(define (endCapture)
  (string-append ve-prefix
                 ve-source
                 ve-suffix
                 ve-modifiers))

(module+ test
  (test))
