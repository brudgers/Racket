;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname quick-select) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Quick Select from Pavel Lepin
;; 

(define L1 (build-list 10 identity))
(define L2 (build-list 10 sqr))


(define (quick-select ix xs)
  (if (empty? (rest xs))
              (car xs)
              (local [(define pivot (first xs))
                      (define (left? x)(< x pivot))
                      (define (right? x)(<= pivot x))
                      (define left (filter left? (rest xs)))
                      (define right (filter right? (rest xs)))
                      (define pivot-ix (+ 1 (length left)))]
                (cond [(= ix pivot-ix) pivot]
                      [(< ix pivot-ix)(quick-select ix left)]
                      [else (quick-select (- ix pivot-ix) right)]))))

(define (median xs)
  (quick-select (floor (/ (+ 1 (length xs)) 2)) xs))