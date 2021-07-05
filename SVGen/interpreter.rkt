#lang racket
(provide execute)

; Global hash table, holds function names, args and their instructions.
(define func_hash_table (make-hash))

; Defines grammar and returns appropriate function to evaluate it.
(define (type_of expr hash_table)
  ;(displayln (format "Hash_table: \"~a\"" hash_table))
  (cond                                     
    ([eqv? expr 'circle] (lambda (cx cy r style) (format "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>" cx cy r style)))
    ([eqv? expr 'line] (lambda (x1 y1 x2 y2 style) (format "<line x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" style=\"~a\"/>" x1 y1 x2 y2 style)))
    ([eqv? expr 'rect] (lambda (x y width height style) (format "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>" x y width height style)))
    ([eqv? expr 'if] "if")
    ([eqv? expr 'when] "when")
    ([eqv? expr '>] >)
    ([eqv? expr '<] <)
    ([eqv? expr '=] =)
    ([eqv? expr '+] +)
    ([eqv? expr '-] -)
    ([eqv? expr '*] *)
    ([eqv? expr '/] /)
    ([eqv? expr 'floor] floor)
    ([eqv? expr 'cos] cos)
    ([eqv? expr 'sin] sin)
    ([symbol? expr] (hash-ref hash_table expr (hash-ref func_hash_table expr #f))) ; get symbol from hash table, can be variable or function
    ([number? expr] expr)
    ([string? expr] expr)
    (else expr)
   )
  )

; Computes numerical expressions, and circle/line/rect expressions (even nested ones)).
(define (calculator expr symbol_table)
  (cond
    ([and (list? expr) (not (empty? expr)) (procedure? (type_of (car expr) symbol_table))]
      (apply (type_of (car expr) symbol_table)
       (for/list ([i (in-range (length (cdr expr)))]) (calculator (list-ref (cdr expr) i) symbol_table)))
    )
    ([and (list? expr) (not (empty? expr))]
     (cons (calculator (car expr) symbol_table)
      (for/list ([i (in-range (length (cdr expr)))])
       (calculator (list-ref (cdr expr) i) symbol_table)))
    )
    (else (type_of expr symbol_table)) ; end of tree, guaranteed to be number or symbol -> number, return
   )
  )

;(define temp '(circle (cos ( + (floor (/ 200 3)) (sin (* (+ 2 5) (- 1 0))))) 200 15 "fill:white;stroke:green"))
;(displayln (calculator temp))



; For each define adds its definition to hash table
(define (process_define lst)
  (cond                                     ; hash_name      ; hash_args     body of function
    ([eqv? (car lst) 'define] (create_hash (car (cadr lst)) (cdr (cadr lst)) (cdr (cdr lst))))
    (else (error "not a define"))
   )
 )

; Inserts hash entry
(define (create_hash hash_name hash_args hash_functions)
  (hash-set! func_hash_table hash_name  (cons hash_args hash_functions))
 )



; Evaluates one expression at a time.
(define (executor expr table)
  ;(displayln (format "Executor, expr: \"~a\", call_num: \"~a\"" expr counter))
  ;(change)
  ; ----- Load instructions ----- 
  (define instruction (car expr)) ; Instruction name
  (define args (cdr expr)) ; arguments
  (define type (type_of instruction table)) ; procedure?,function?,if?,when?
  ;(displayln (format "Instruction: \"~a\"" instruction)) ; current function/procedure/if/when
  ;(displayln (format "Instruction args: \"~a\"" args)) ; current args
  ;(displayln type) ; type of expr
  ; ----- Process instructions ----- 
  (cond    
    ([list? type] ; Function
                  ;(displayln "Proceeding to process function")
                  (define f_args (car type)) ; function arguments 
                  (define f_input (calculator args table)) ; function inputs (can be equations)
                  ;(displayln (format "Function args: \"~a\"" f_args))
                  ;(displayln (format "Function inputs: \"~a\"" f_input))
                  ;(displayln "Creating hash-table for args")
                  (define symbol_table (make-hash))
                  (for/list ([i (in-range (length f_args))]) (hash-set! symbol_table (list-ref f_args i) (list-ref f_input i)))
                  ;(displayln (format "Hash table with args:  \"~a\"" symbol_table))
                  ;(displayln (format "Calling executor with instructions:  \"~a\"" (cdr type)))
                  ;(display "\n")
                  ;(executor (cdr type) symbol_table acc)
                  (for/list ([i (in-range (length (cdr type)))]) (executor (list-ref (cdr type) i) symbol_table)) ; new instructions
                  ;acc
     ) 
    ([procedure? type] ; Procedure -> rect/line/circle
                 ;(displayln "Proceeding to process procedure")
                 ;(displayln (calculator expr table))
                 ;(displayln (calculator expr table) out)
                 ;(display "\n")
                 (calculator expr table)
     )                       ; check <bool-expr>
    ([and (eqv? type "if") (calculator (cadr expr) table)] ; if evaluated to true
                  ;(displayln (format "If <bool-expr>: \"~a\" evaluated to \"~a\"" (cadr expr) (calculator (cadr expr) table)))
                  ;(displayln (format "If <than-expr>: \"~a\"" (caddr expr)))
                  ;(display "\n")
                  (executor (caddr expr) table) ; process than-expr
     )                        ; check <bool-expr>
    ([and (eqv? type "if") (not (calculator (cadr expr) table))] ; if evaluated to false
                  ;(displayln (format "If <bool-expr>: \"~a\" evaluated to \"~a\"" (cadr expr) (calculator (cadr expr) table)))
                  ;(displayln (format "If <else-expr>: \"~a\"" (last expr)))
                  ;(display "\n")
                  (executor (last expr) table) ; process else-expr
     )                       ; check <bool-expr>
    ([and (eqv? type "when") (calculator (cadr expr) table)] ; when evaluated to true
                  ;(displayln (format "When <bool-expr>: \"~a\" evaluated to \"~a\"" (cadr expr) (calculator (cadr expr) table))) 
                  ;(displayln (cddr expr))
                  ;(display "\n")
                  (for/list ([i (in-range (length (cddr expr)))]) (executor (list-ref (cddr expr) i) table)) ; the rest of current instructions
     )
    ([and (eqv? type "when") (not (calculator (cadr expr) table))] ; when evaluated to false
                  ;(displayln (format "When <bool-expr>: \"~a\" evaluated to \"~a\"" (cadr expr) (calculator (cadr expr) table)))
                  ""
     )
    (else (error "Unknown"))
    )
  )

; SVG interpreter
(define (execute width height prg expr)
  (for/list ([i (in-range (length prg ))]) (process_define (list-ref prg i))) ; create global hash table with functions
  ;(define array '())
  ;(displayln (format "<svg width=\"~a\" height=\"~a\">" (calculator width func_hash_table) (calculator height func_hash_table)))
  (define array (list (format "<svg width=\"~a\" height=\"~a\">" (calculator width func_hash_table) (calculator height func_hash_table))))
  ;(displayln (format "<svg width=\"~a\" height=\"~a\">" (calculator width func_hash_table) (calculator height func_hash_table)))
  ;(displayln "printing hash_func_table")
  ;(displayln func_hash_table )
  (define func (type_of (car expr) func_hash_table))
  (define result (flatten (executor expr func_hash_table)))
  (define pre-result (append array result))
  (string-append* "" (append pre-result (list "</svg>")))
  ;(displayln "Done")
 )


#|
; Test 1.
;(display (execute 400 400 '() '(line 10 20 30 40 "stroke:black;stroke-width:5")))


; Test 2.
;(display (execute 400 400 '() '(circle 200 200 (floor (/ 200 3)) "fill:red")))

; Test 3.
(define test1
  '(
    (define (start)
      (rect 0 0 100 100 "fill:red")
      (rect 100 0 100 100 "fill:green")
      (rect 200 0 100 100 "fill:blue")
      )
    )
  )
(display (execute 400 400 test1 '(start)))

; Test 4.
(define test2
  '(
    (define (circles x r)
      (when (> r 10)
        (circle x 200 r "fill:white;stroke:green")
        (circles (+ x (floor (/ r 2.0))) (floor (/ r 2)))
        )
      )
    )
   )

; Test 5
(define tree-prg
  '(
    (define (draw x1 y1 x2 y2 len angle)
      (if (> len 30)
          (line x1 y1 x2 y2 "stroke:black;stroke-width:2;opacity:0.9")
          (line x1 y1 x2 y2 "stroke:green;stroke-width:3;opacity:0.9")
          )
      (when (> len 20)
        (recur-tree x2 y2 (floor (* len 0.7)) angle)
        (recur-tree x2 y2 (floor (* len 0.7)) (+ angle 0.3))
        (recur-tree x2 y2 (floor (* len 0.7)) (- angle 0.6))
        )
      )
    (define (recur-tree x1 y1 len angle)
      (draw x1
            y1
            (+ x1 (* len (cos angle)))
            (+ y1 (* len (sin angle)))
            len
            angle
            )
      )
    )
  )

(display (execute 400 400 tree-prg '(recur-tree 200 400 100 (* 3.14 1.5))))
|#
