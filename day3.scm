(load "utils.scm")

(define parse-number
  (lambda (line index number)
    (let [(eval (lambda (list) (string->number (list->string number))))]
      (cond
        [(>= index (string-length line)) (eval number)]
        [(not (char-numeric? (string-ref line index))) (eval number)]
        [#t (parse-number line (+ index 1) (append number (list (string-ref line index))))]))))

(define parse-line
  (lambda (line index . result)
    (let [
        (result (if (null? result)
          (make-vector (string-length line) '())
          (car result)))
        (len (string-length line))
        (recurse (lambda (result) (parse-line line (+ index 1) result)))
      ]
      (cond
        ; Done processing the line
        [(>= index len) result]
        ; We don't care to process dots
        [(char=? #\. (string-ref line index)) (recurse result)]
        ; This is a number, so parse if we're at the beginning or just extend it
        [(char-numeric? (string-ref line index))
          (begin
            ; O no side effects :(
            (vector-set! result index
              (if (or (eq? index 0) (not (number? (vector-ref result (- index 1)))))
                ; The current number has not been parsed, parse
                (parse-number line index '())
                ; The current number has been parsed, so just use that
                (vector-ref result (- index 1))))
            (recurse result))]
        ; Assuming anything other than a . or a numeric is a symbol
        [#t
          (begin
            (vector-set! result index #t)
            (recurse result))]
          ))))

; Returns the input list with any false elements removed
(define filter-if-false
  (lambda (input)
    (if (null? input)
      '()
      (if (car input)
        (cons (car input) (filter-if-false (cdr input)))
        (filter-if-false (cdr input))))))

; Produces a list of the product of both provided lists.
;
; Input lists are pairs of condition and value;
; Output is a list of false, or (a_value b_value) iff (a_cond && b_cond)
(define weird-product
  (lambda (a b)
    (if (not (null? a))
      ; There are still elements in a to evaluate
      (append
        (let inner [(a (car a)) (b b)]
          (if (not (null? b))
            ; There are still elements in b to evaluate
            (cons
              (if (and (car a) (caar b))
                ; Both condition are true, return the composite value
                (list (cadr a) (cadar b))
                ;Conditions are false, return false)
                #f)
              (inner a (cdr b)))
            ; The bottom of our inner loop
            '()))
        (weird-product (cdr a) b))
      ; The bottom of our outer loop
      '())))

;;; Returns a list of valid neighbours provided an x-y coordinate, and the dimensions of the total array
(define get-neighbouring-cells
  (lambda (dimensions position)
    (let [
        (xlim (- (car dimensions) 1))
        (ylim (- (cadr dimensions) 1))
        (x (car position))
        (y (cadr position))
      ]
      (let [
          (xmore (list (> x 0) (- x 1))) ; if checking, (- x 1)
          (xless (list (< x xlim) (+ x 1))); if checking, (+ x 1)
          (xeq (list #t x))
          (ymore (list (> y 0) (- y 1))) ; if checking, (- y 1)
          (yless (list (< y ylim) (+ y 1))); if checking, (+ y 1)
          (yeq (list #t y))
        ]
        (filter-if-false (weird-product (list xmore xeq xless) (list ymore yeq yless)))))))

(define get-neighbours
  (lambda (lines dimensions position)
    (let loop [
        (neighbours (get-neighbouring-cells dimensions position))
      ]
      (if (null? neighbours)
        '()
        (let [
            (x (caar neighbours))
            (y (cadar neighbours))
          ]
          (let [(neighbour (vector-ref (vector-ref lines y) x))]
            (if (number? neighbour)
              (cons neighbour (loop (cdr neighbours)))
              (loop (cdr neighbours)))))))))

(define contains?
  (lambda (list search-item)
    (if (null? list)
      #f
      (if (eq? (car list) search-item)
        #t
        (contains? (cdr list) search-item)))))

(define remove-neighbouring-duplicates
  (lambda (neighbours)
    ; Nothing more to in the list?
    (if (null? neighbours)
      '()
      ; If there's no more bits left in the list?
      (if (null? (cdr neighbours))
        neighbours
        ; We have this in the list
        (if (contains? (cdr neighbours) (car neighbours))
          (remove-neighbouring-duplicates (cdr neighbours))
          (cons (car neighbours) (remove-neighbouring-duplicates (cdr neighbours))))))))

;;; Adds all neighbouring tiles that are numbers
(define add-neighbours
  (lambda (lines dimensions position)
    (let loop [
      (neighbours
        (remove-neighbouring-duplicates
          (get-neighbours lines dimensions position)))
      ]
      (if (null? neighbours)
        0
        (+
          (car neighbours)
          (loop (cdr neighbours)))))))

(define multiply-two-neighbours
  (lambda (lines dimensions position)
    (let [
        (neighbours
          (remove-neighbouring-duplicates
            (get-neighbours lines dimensions position)))
      ]
      (if (= (length neighbours) 2)
        (* (car neighbours) (cadr neighbours))
        0))))

(define input
  (list->vector
    (let parse-lines [
          (file (open-input 3))
        ]
        (if (port-eof? file)
          '()
          (cons
            (parse-line (get-line file) 0)
            (parse-lines file))))))


(define calculate
  (lambda (subcalc lines)
    (let [
        (xsize (vector-length (vector-ref lines 0)))
        (ysize (vector-length lines))
      ]
      (let loop [
          (x 0)
          (y 0)
        ]
        ; If we've passed the y bounds, we are done iterating
        (if (>= y ysize)
         0
          (if (>= x xsize)
            (loop 0 (+ y 1))
            (+
              (if (boolean? (vector-ref (vector-ref lines y) x))
                (subcalc lines (list xsize ysize) (list x y))
                0)
              (loop (+ x 1) y))))))))

(begin
  (display "Part 1 results: ")
  (display (calculate add-neighbours input))
  (display "\n"))

(begin
  (display "Part 2 results: ")
  (display (calculate multiply-two-neighbours input))
  (display "\n"))
