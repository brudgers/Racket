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
(define (fizz-buzzer fizz-pair buzz-pair start stop)
  (let* (;; (int . string) -> (int . string) | raises error
         ;; validates 'fizz-pair and 'buzz-pair
         [validate-pair 
          (lambda (pr name)
            (let ([int (integer? (car pr))]
                  [string (string? (cdr pr))])
              (if (not int)
                  (error 'fizz-buzzer "first element of ~a cannot be coerced to an integer" name)
                  (if (not string)
                      (error 'fizz-buzzer "second element of ~a not a string" name)
                      pr))))]
         ;; integer -> integer | raises error
         ;; validates 'start and 'stop
         [validate-int
          (lambda (i name)
            (if (integer? i)
                i
                (error 'fizz-buzzer " ~a value cannot be coerced to an integer" name)))]
         ;; note that Racket documentation uses
         ;; 'car and 'cdr for dotted pair examples
         ;; so following it here [see Racket Guide 3.8]
         [fizz-val  (car (validate-pair fizz-pair "fizz-pair"))]
         [fizz-word (cdr fizz-pair)]
         [buzz-val  (car (validate-pair buzz-pair "buzz-pair"))]
         [buzz-word (cdr buzz-pair)]
         ;; implicit fizzbuzz pair
         [fizzbuzz-val (* fizz-val buzz-val)]
         [fizzbuzz-word (string-append
                         fizz-word buzz-word)]
         ;; generate the range of integers over which to iterate
         [iterations (range (validate-int start "start")
                            (+ (validate-int stop "stop") 1))]  ; 'range output not inclusive
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


;;; list -> side effect
;;; prints the contents of a list without 
;;; the enclosing parenthesis
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


;;; -> side effect
;;; a classic fizzbuzz using "Crackle" and "Pop"
(define crackle-pop
  (lambda ()(print-list(fizz-buzzer '(3 . "Crackle")
                                    '(5 . "Pop") 
                                    1 100))))

(crackle-pop)
