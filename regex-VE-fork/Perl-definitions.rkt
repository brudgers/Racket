#lang racket

;; Perl-definitions from
;; http://perldoc.perl.org/perlre.html

;; Modifiers

;; find ^ and $ anywhere in string
(define m "treat string as multiple lines")

;; treat string as single line
;; means that . matches every character
;; including "newline"
(define s "treat string as single line")

;; using /ms lets . match any character
;; but also allowing ^ and $ to match just
;; before and after new lines respectively
(define ms ". matches any character but ^ and $ still match correctly")

;; case insensitive matching
(define i "case insensitive")

;; allow whitespace and comments
(define x "whitespace and comments allowed")

;; preserve strings
(define preserve "create $prematch $match $postmatch")

;; allow global matching - effects the way
;; regexp is used rather than the regex
(define g "global matching")

;; keep current position
;; effects the way regexp is used rather
;; than the regex
(define p "keep current position")

;; allow ASCII safe unicode matching
(define a "use ASCII safe matching")

;; use default character set
;; problematic
(define d "use default character set")

;; use unicode character set
(define u "use Unicode character set")

;; use local character set
(define l "use local character set")

;; Metacharacters

;; !!!
(define (veQuote str)
  (string-append "\\" str))

(define beginning-of-line "^")

(define any-character  ".") ;; except new-line

(define end-of-line "$")

(define begin-alternates "(")

(define end-alternates ")")

(define end-alternate "|")

(define begin-group "(")

(define end-group ")")

(define begin-class "[")

(define end-class "]")

;; Quantifiers

(define match-zero-or-more "*")

(define match-one-or-more "+")

(define match-one-or-zero "?")

(define (match-exactly-times n)
  (string-append "{"
                 (number->string n)
                 "}"))

(define (match-at-least-times n)
  (string-append "{"
                 (number->string n)
                 ",}"))

(define (match-between-times n m)
  (string-append "{"
                 (number->string n)
                 ","
                 (number->string n)
                 "}"))

(define (match-at-most-times m)
  (string-append "{,"
                 (number->string m)
                 "}"))

;; need possive !!!
;;      'aaaa' =~ /a++a/
;; will never match, as the a++ will gobble up 
;; all the a 's in the string and won't leave 
;; any for the remaining part of the pattern

;;;;;;;;;;;;;;;;;;;;
;; Escape Sequences
;;;;;;;;;;;;;;;;;;;;


(define tab "\\t")

(define newline "\\n")

(define return "\\r")

(define formFeed "\\f")

(define alarm "\\a")

(define escape "\\e")

(define controlChar "\\cK")

;; !!!
;; hexadecimal

;; !!!
;; named unicode character

;; !!!
;; unicode character

;; !!!
;; charcter whose ordinal is an octal

(define lowerCaseCharacterNext "\\l")

(define upperCaseCharacterNext "\\u")

(define lowerCaseUntilEnd "\\L")

(define upperCaseUntilEnd "\\U")

(define quoteUntilEnd "\\Q")

(define endEscape "\\E")

;;;;;;;;;;;;;;;;;;;;
;; Character Classes
;;;;;;;;;;;;;;;;;;;;


;; !!! needs to be a macro
(define (bracketedClass rgx)
  (string-append "["
                 rgx
                 "]"))

(define (posixClass pc)
  (string-append "[[:"
                 pc
                 ":]]"))

;; !!! future use
;; extended bracketed character
;; class















