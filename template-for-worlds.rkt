;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname template-for-worlds) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

;; My world program  (make this more specific)

;; =================
;; Constants:


;; =================
;; Data definitions:

;; WS is ... (give WS a better name)



;; =================
;; Functions:

;; WS -> WS
;; start the world with ...
;; 
(define (main ws)
  (big-bang ws                   ; WS
            (on-tick   tock)     ; WS -> WS
            (to-draw   render)   ; WS -> Image
            (stop-when last-world?)      ; WS -> Boolean
            (on-mouse  handle-mouse)      ; WS Integer Integer MouseEvent -> WS
            (on-key    handle-key)))    ; WS KeyEvent -> WS

;; WS -> WS
;; produce the next ...
;; There is no time progression
(define (tock ws) ws)

;; WS -> Nil
;; produce last world state 
(define (last-world? ws)
  (ws-last-world))


;; WS -> Image
;; render ... 
;; !!!
(define (render ws) ...)


;; WS MouseEvent -> WS
;; !!!

(define (handle-mouse ws x y me)
  (cond [(mouse=? me "button-down") (... ws x y)]
        [else
         (... ws x y)]))

;; WS KeyEvent -> WS
;; !!!

(define (handle-key ws ke)
  (cond [(key=? ke " ") (... ws)]
        [else 
         (... ws)]))

