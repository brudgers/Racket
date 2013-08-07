#lang racket
(module+ test
  (require test-engine/racket-tests))

(provide (all-defined-out))

(define ve-prefix "")
(define ve-source "")
(define ve-suffix "")
(define ve-modifiers "")
(define ve-replace empty)


(define (reset)
  (set! ve-prefix "")
  (set! ve-source "")
  (set! ve-suffix "")
  (set! ve-modifiers "")
  (set! ve-replace empty))





;; String -> String
;; prepends string to prefix portion of ve
(define(next-pre p)
  (set! ve-prefix 
        (string-append p ve-prefix)))



;; String -> String
;; appends string to src portion of ve
(define(next-src s)
  (set! ve-source 
        (string-append ve-source s)))

;; VE String -> String
;; appends string to mod portion of ve
(define(next-mod m)
  (set! ve-modifiers
        (string-append ve-modifiers m)))

;; String -> String
;; appends string to suffix portion of ve
;; !!! check use of ""
(define(next-suf s)
  (if (regexp-match? s ve-suffix)
      ""
      (set! ve-suffix (string-append ve-suffix s))))

(module+ test
  (check-expect (begin (next-pre "pre")
                       (regex))
                "pre")
  (check-expect (begin (next-src "src")
                       (regex))
                "presrc")
    (check-expect (begin (next-suf "suf")
                       (regex))
                "presrcsuf")
    (check-expect (begin (next-mod "mod")
                       (regex))
                "presrcsufmod")
  (reset))

;; String  -> String
;; replaces all occurances of 
;; does not handle nested replacement
;; !!!
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
