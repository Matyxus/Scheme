#lang racket
(require 2htdp/image)
(provide img->mat ascii-art)

; make image grey-scale
(define (RGB->grayscale color)
  (+ (* 0.3 (color-red color))
     (* 0.59 (color-green color))
     (* 0.11 (color-blue color)))
)

; create matrix from list of color intensities
(define (maker width acc lst)
  (cond ((null? lst) acc)
        (else (maker width (append acc (list (take lst width))) (drop lst width)))
   )
 )

; takes image and transoforms is into matrix,
; rows are same length as the image-width
(define (img->mat img)
  (cond ((null? img) (displayln "error no image detected"))
        (( < (image-width img) 1) (displayln "error img-width is smaller than 1"))
        ; maker creates matrix from list of grey color intensities, length of row = image-width
        (else (maker (image-width img) '() (map RGB->grayscale (image->color-list img) )) )
     )
 )

; sums part of rows depending on the chosen block-width,
; in case block-width is smaller than length of row,
; its splitted floor((length row )/(block-width)) times,
; values that would not fit into block are discarded
(define (row-sum step acc row)
  (cond ((null? row) acc)
        (( > step (length row)) acc)
        (else (row-sum step (append acc (list (apply + (take row step)))) (drop row step)))
    )
  )

; add cols, from already summed (compressed) rows
; values that would not fit are discarded same as in
; row-sum
(define (add-colls amount divisor acc  list-list)
  (cond ((> amount (length list-list)) acc)
        (else (add-colls amount divisor (append acc (list (map (lambda ( number) (/ number divisor)) (apply map + (take list-list amount))))) (drop list-list amount)))
    )
  )

; creates blocks from matrix intensities,
; intensities in blocks are averaged
(define (blocks width heigth img-matrix)
  (define divisor ( * width heigth))
  (define together (for/list ([i (in-range (length img-matrix))]) (row-sum width '() (list-ref img-matrix i))))
  (add-colls heigth divisor '() together)
 )

; convert color intensities to characters using formula:
; chars[k], where k =  exact-floor ( (255 - floor(intensity)) / 256 )
(define (convert lst chars)
  (for/list ([i (in-range (length lst ))]) (string-ref chars  (exact-floor(/  (* (string-length chars) (- 255 (floor(list-ref lst i)))) 256 ))))
  )

; convert matrix of string to single list of strings
(define (process lst)
  (for/list ([i (in-range (length lst ))]) (apply string-append (map string (list-ref lst i))))
 )

(define (helper width height chars img)
  ; matrix of averaged blocks made from image intensities
  (define result (blocks width height (img->mat img)))
  ; matrix of characters, one for each number
  (define converted (for/list ([i (in-range (length result ))]) (convert (list-ref result i) chars)))
  ; single list of strings
  (define processed (process converted))
  ; add new-lines for each row
  (define added-lines (for/list ([i (in-range (length processed))]) (string-append (list-ref processed i) "\n")))
  ; append all strings to one final
  (string-append* "" added-lines)
 )



(define (ascii-art width height chars)
  (lambda (i) (helper width height chars i))
  )
