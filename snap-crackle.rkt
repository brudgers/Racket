#lang racket
;;; DataType FizzBuzz
;;; is int or string


;;; (int . string)(int . string) int int -> listof FizzBuzz
;;; Function fizz-buzzer :
;;; fizz-buzzer is a general function for solving 
;;; fizzbuzz type problems with two explicit conditions:
;;; [fizz and buzz] and one implicit condition [fizzbuzz]
;;;      An interesting extension would be solving fizzbuzz problems for
;;;      any arbitrary number of pairs.
;;; the first argument is a pair consisting of
;;;    + an int upon which an input argument should fizz
;;;    + the string which a fizz should return
;;; the second argument is a pair consisting of
;;;    + an int upon which an input argument should buzz
;;;    + the string which a buzz should return
;;; the third argument is the lower bound of the fizzbuzz's range
;;; the fourth argument is the upper bound of the fizzbuzz's range
(define (fizz-buzzer pr1 pr2 start stop)
  (let* ([iterations (range start (+ stop 1))]  ; 'range output not inclusive
         ;; note that Racket documentation uses
         ;; 'car and 'cdr for dotted pair examples
         ;; so following it here [see Racket Guide 3.8]
         [fizz-val  (car pr1)]
         [fizz-word (cdr pr1)]
         [buzz-val  (car pr2)]
         [buzz-word (cdr pr2)]
         ;; implicit fizzbuzz pair
         [fizzbuzz-val (* fizz-val buzz-val)]
         [fizzbuzz-word (string-append
                         fizz-word buzz-word)]
         ;; int -> FizzBuzz
         ;; a helper for our map function
         [aux (lambda(int)
                (cond [(= int 0) int]
                      [(= (modulo int fizzbuzz-val) 0)
                       fizzbuzz-word]
                      [(= (modulo int fizz-val) 0)
                       fizz-word]
                      [(= (modulo int buzz-val) 0)
                       buzz-word]
                      [else int]))])
    ;; return a list of FizzBuzz
    (map aux iterations)))


;; list -> side effect
;; prints the contents of a list without 
;; the enclosing parenthesis
(define (print-list lst)
  (letrec 
       ;; any -> string
       ;; string ends with a newline character
      ([formatter (lambda (x) (format "~a ~n" x))]
       ;; listof any -> string
       [aux (lambda (lst)
              (if (null? lst)
                  ""
                  (string-append (formatter (car lst))
                                 (aux (cdr lst)))))])
    (display (aux lst))))


;; -> side effect
;; a classic fizzbuzz using "snap" and "crackle"
(define snap-crackle 
  (lambda ()(print-list(fizz-buzzer '(3 . "snap")
                                    '(5 . "crackle") 
                                    1 100))))

(snap-crackle)
