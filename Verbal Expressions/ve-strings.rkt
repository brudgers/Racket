;; TODO
;; Allow Nested Replacement?

;; ve-strings.rkt
;; contains core data structures
;; - the functions to directly set them
;; - (regex) to return the regex as a string

#lang racket
(module+ test
  (require test-engine/racket-tests))

(provide (all-defined-out))

(define ve-prefix "")
(define ve-source "")
(define ve-suffix "")
(define ve-modifiers "")
;!!! needs full implementation
(define ve-replace empty) 


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
        (string-append str ve-prefix)))



;; String -> String
;; appends string to src portion of ve
(define(next-src str)
  (set! ve-source 
        (string-append ve-source str)))



;; String -> SideEffect
;; appends string to mod portion of ve
(define(next-mod str)
  (cond [(string=? "" ve-modifiers)
         (set! ve-modifiers str)]
        [(regexp-match? str ve-modifiers)
         ve-modifiers]
        [else
         (set! ve-modifiers 
               (string-append ve-modifiers str))]))

;; String -> SideEffect
;; appends string to suffix portion of ve
(define(next-suf str)
  (cond [(string=? "" ve-suffix)
         (set! ve-suffix str)]
        [(regexp-match? str ve-suffix)
         ve-suffix]
        [else
         (set! ve-suffix 
               (string-append ve-suffix str))]))

(module+ test
  ;; multi-unit test
  (check-expect (begin (reset)
                       (next-pre "^")
                       (next-src ";;")
                       (next-suf "$")
                       (next-mod "i")
                       (regex))
                (string-append "^"";;""$""i"))

  (check-expect (begin (reset)
                       (next-pre "^")
                       (next-src ";;")
                       (next-suf "$")(next-suf "$")
                       (next-mod "i")(next-mod "i")
                       (regex))
                (string-append "^"";;""$""i"))
    
  (check-expect (begin (reset)
                       (next-pre "^")
                       (next-src ";;")
                       (next-suf "$")(next-suf "k")
                       (next-mod "i")(next-mod "k")
                       (regex))
                (string-append "^"";;""$k""ik")))

;; String  -> String
;; replaces all occurances of 
;; does not handle nested replacement
;; ^^^^^^^^^^^^^^^^^ Kludge
(define (replace f r)
  (set! ve-replace (list f r)))

;; -> String
;; return the regular expression
(define (regex)
  (string-append ve-prefix
                 ve-source
                 ve-suffix
                 ve-modifiers))

(module+ test
  (test))
