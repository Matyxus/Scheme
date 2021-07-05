#lang racket
(provide rotationInstructions)

; reverses row if index is equal to row_id
(define (reverse-row  dir index row row_id)
  ;(displayln (format "Reversing row at index: \"~a\"" index))
  ;(displayln (format "Row id \"~a\"" row_id))
  ;(displayln (format "Row \"~a\"" row))
  ;(displayln (format "Dir \"~a\"" dir))
  (cond
    ([eqv? index row_id] (cond
                           ([eqv? dir 'right] (define removed (list (list-ref row (- (length row)  1))))
                                              (define tmp (take row (- (length row) 1)))
                                              (define appended (append removed tmp))
                                              appended ; return
                                              )
                           (else (define removed (list (car row)))
                                 (define appended (append  (cdr row) removed))
                                 appended ; return

                                 )
                           )
                         )
                         
    (else row)
    )
 )


(define (process-row matrix index dir)
  ;(displayln (format "Matrix: \"~a\"" matrix))
  ;(displayln (format "Index to reverse: \"~a\"" index))
  (define result (for/list ([i (in-range (length matrix))]) (reverse-row dir index (list-ref matrix i) i)))
  ;(displayln result)
  result
  )

; recursively transposes rows into cols
(define (helper matrix id index curr acc)
  (cond
    ([eqv? curr 0] acc)
    (else 
           ;(displayln (format "At row: \"~a\"" id))W
           ;(displayln (format "At element: \"~a\"" index))
           ;(displayln (format "Acc: \"~a\"" acc))
           ;(displayln (format "Target: \"~a\"" (list-ref (list-ref matrix id) index)))
           (helper matrix (+ id 1) index (- curr 1) (append acc (list (list-ref (list-ref matrix id) index))))) 
    )
  )

; Transposes matrix
(define (transpose matrix)
  (define len (length (list-ref matrix 0)))
  ;(displayln len)
  (for/list ([i (in-range len)]) (helper matrix 0 i (length matrix) '()))
  )



; Switch based on instructions
(define (compute matrix instr)
  (define inst (car instr))
  (define index (car (cdr instr)))
  (cond
    ([eqv? inst 'left] (define temp (process-row matrix index inst))
                       ;(displayln (format "Left rotation: \"~a\"" temp))
                       temp
                       )
    ([eqv? inst 'right] (define temp (process-row matrix index inst))
                        ;(displayln (format "Right rotation: \"~a\"" temp))
                        temp
                        )
    ([eqv? inst 'up] (define temp (transpose (process-row (transpose matrix) index 'left)))
                     ;(displayln (format "Up rotation: \"~a\"" temp))
                     temp
                     )
    ; down
    (else (define temp (transpose (process-row (transpose matrix) index 'right)))
          ;(displayln (format "Down rotation: \"~a\"" temp))
          temp
          ) ; down
    )
  )

; Recursively takes instructions from instr list and
; processes them
(define (work matrix instr depth)
  (cond
    ([eqv? depth (length instr)] matrix)
    (else (work (compute matrix (list-ref instr depth)) instr (+ depth 1)))
    )
  )

; Main function
(define (rotationInstructions matrix instr)
  ;(displayln (format "Matrix: \"~a\"" matrix))
  ;(displayln (format "Instructions: \"~a\"" instr))
  (work matrix instr 0)
  )

;(rotationInstructions m instr)



#| Example 1, 2:
(define m '((1 2 3)(4 5 6)))
(define instr0 '((left 0)))
(define instr1 '((down 0)))
Result 1:
(rotationInstructions m instr0)
'((2 3 1) (4 5 6))
Result 2:
(rotationInstructions m instr1)
'((4 2 3) (1 5 6))
; Example 3:
(define m1 '((1 2)(3 4)))
(define instr1 '((up 1)(right 1)))
; Result 3:
(rotateInstructions m1 instr1)
'((1 4) (2 3))
; Example 4:
(define m '((1 2 3)(4 5 6)(7 8 9)))
(define instr '((left 1)(down 2)(right 2)))
; Result 4:
(rotationInstructions m instr)
'((1 2 9) (5 6 3) (4 7 8))
|#





