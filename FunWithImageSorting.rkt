;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname FunWithImageSorting) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)

;; ================
;; Constants

(define BLANK (square 0 "solid" "white"))

(define LOI (list BLANK
                  (square 10 "solid" "blue")
                  (square 15 "solid" "red")
                  (square 20 "solid" "blue")
                  (square 30 "solid" "red")))

;; ================
;; Data Definitions


;; A Graphic is an image along with the area of
;; the image and the perimeter of the image
(define-struct graphic (pic area perimeter))


;; ================
;; Functions

;; (listof Image) -> (listof Graphic)
;; turns a list of images to a list of graphics

(check-expect (new-log empty)empty)

(check-expect (new-log (list BLANK))
              (list (make-graphic BLANK 0 0)))


(define (new-log loi)
  (local [(define (new-graphic i)
          (make-graphic i (area i)(perimeter i)))
  (define (area i)
            (* (image-height i)(image-width i)))
          
          (define (perimeter i)
            (* 2 (+ (image-height i)
                    (image-width i))))]
  (map new-graphic loi)))


;; (listof Graphic) -> (listof Images)
;; unboxes images from graphic structure

(check-expect (unbox-images LOG)LOI)
(check-expect (unbox-images (reverse LOG))(reverse LOI))


(define (unbox-images log)
  (local [(define (graphic->image g)
            (graphic-pic g))]
    (map graphic->image log)))

;; Make a list of graphics for testing
(define LOG (new-log LOI))
(define LOG2 (rest LOG))

;; Graphic Graphic (Graphic -> Number) (Number Number -> Boolean) -> Boolean
;; compares to Graphics using a field and returns true if they 
;; the predicate is true

(check-expect (compare-graphic (second LOG)
                               (third LOG)
                               graphic-perimeter
                               >)
              false)

(check-expect (compare-graphic (second LOG)
                               (third LOG)
                               graphic-perimeter
                               <)
              true)
(check-expect (compare-graphic (second LOG)
                               (third LOG)
                               graphic-area
                               >)
              false)

(check-expect (compare-graphic (second LOG)
                               (third LOG)
                               graphic-area
                               <)
              true)

(define (compare-graphic a b field p?)
  (p? (field a) (field b)))

;; Graphic Graphic -> Boolean
;; Two Functions for comparing perimeter
;; or area

(check-expect (smaller-perimeter? (third LOG)
                                  (second LOG))
              false)
(check-expect (smaller-perimeter? (second LOG)
                                  (third LOG))
              true)
(check-expect (smaller-area? (third LOG)
                             (second LOG))
              false)
(check-expect (smaller-area? (second LOG)
                             (third LOG))
              true)


(define (smaller-perimeter? a b)
  (compare-graphic a b graphic-perimeter <=))

(define (larger-perimeter? a b)
  (not (smaller-perimeter? a b)))

(define (smaller-area? a b)
  (compare-graphic a b graphic-area <=))

(define (larger-area? a b)
  (not (smaller-area? a b)))


;; (listof Graphic) (Number Number -> Boolean) -> (listof Graphic)
;; sorts a list of graphics on Graphics Predicate

(check-expect (fn-sort-graphic LOG smaller-area?) LOG)
(check-expect (fn-sort-graphic (reverse LOG) smaller-area?) LOG)
(check-expect (fn-sort-graphic LOG larger-area?) (reverse LOG))
(check-expect (fn-sort-graphic (reverse LOG) larger-area?) (reverse LOG))


(define (fn-sort-graphic log p?)
  (local [(define (insert x lox)
            (cond [(empty? lox) (cons x empty)]
                  [else
                   (if (p? x (first lox))
                       (cons x lox)
                       (cons (first lox)
                             (insert x
                                     (rest lox))))]))]
    (cond [(empty? log) empty]
          [else
           (insert (first log)
                   (fn-sort-graphic (rest log) p?))])))

;; (listof Graphic) -> (listof Graphic)
;; sorts a list of graphics on area

(check-expect (sort-smaller-area LOG) LOG)
(check-expect (sort-smaller-area (reverse LOG)) LOG)


(define (sort-smaller-area log)
  (fn-sort-graphic log smaller-area?))

(check-expect (sort-larger-area LOG) (reverse LOG))
(check-expect (sort-larger-area (reverse LOG)) (reverse LOG))

(define (sort-larger-area log)
  (fn-sort-graphic log larger-area?))

;; (listof Graphic) -> (listof Graphic)
;; sorts a list of graphics on perimeter
(check-expect (sort-smaller-perimeter LOG) LOG)
(check-expect (sort-smaller-perimeter (reverse LOG)) LOG)


(define (sort-smaller-perimeter log)
  (fn-sort-graphic log smaller-perimeter?))

(check-expect (sort-larger-perimeter LOG) (reverse LOG))
(check-expect (sort-larger-perimeter (reverse LOG)) (reverse LOG))

(define (sort-larger-perimeter log)
  (fn-sort-graphic log larger-perimeter?))


;; (listof Graphic) -> Graphic
;; functions to produce the Graphic with a property

(define (smallest-area log)
  (first (sort-smaller-area log)))

(define (largest-area log)
  (first (reverse (sort-smaller-area log))))

(define (smallest-perimeter log)
  (first (sort-smaller-perimeter log)))

(define (largest-perimeter log)
  (first (reverse (sort-smaller-perimeter log))))

;; (listof Graphic) -> Graphic
;; functions to return median size graphic from a list of Graphics

(check-expect (median-perimeter LOG)
              (make-graphic (square 15 "solid" "red")
                            (* 15 15)
                            (* 4 15)))

(define (median-perimeter log)
  (list-ref (sort-smaller-perimeter log) 
            (round(/ (length log) 2)))) ; (round x) rounds to even


(check-expect (median-area LOG)(make-graphic (square 15 "solid" "red")(* 15 15)(* 4 15)))

(define (median-area log)
  (list-ref (sort-smaller-area log) 
            (round(/ (length log) 2)))) ; (round x) rounds to even

