;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |soduko template notes|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; template for mutual reference
;; 
;; When there is generative recursion,
;; fn-for-x is replaced with 
;; the template for generative recursion.
;;
;; When there is a backtracking search,
;; fn-for-lox is replaced with
;; the template for backtracking search

#;
(define (fn-for-x x)
  (... (x-proprety1 x)
       (fn-for-lox (x-subs x))))         ; mutual recursion from mutual-reference

#;
(define (fn-for-lox lox)
  (cond [(empty? lox) ...]
        [else
         (... (fn-for-x (first lox))       ;mutual recursion from mutual-reference
              (fn-for-lox (rest lox)))]))  ;natural recursion from self-reference


;; template for generative recursion
#;
(define (genrec-fn x)
  (if (trivial? x)
      (trivial-answer x)
      (... x 
           (genrec-fn (next-problem x)))))  ;; in mutual self reference
                                            ;; genrec-fn is fn-for-lox

;; template for backtracking search
#;
(define (backtracking-fn x)
  (local [(define (fn-for-x x)
            (... (fn-for-lox (x-subs x))))
          
          (define (fn-for-lox lox)
            (cond [(empty? lox) false]
                  [else
                   (local [(define try (fn-for-x (first lox)))] ;try first child
                     (if (not (false? try))                     ;successful?
                         try                                    ;if so produce that
                         (fn-for-lox (rest lox))))]))]          ;or try rest of children
    
    (fn-for-x x)))
