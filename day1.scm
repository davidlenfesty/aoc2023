;;; Read all lines from stdin and print them back
(define readlines
  (lambda (line)
    (cond
      [(eof-object? line) #f]
      [(null? line) (begin
        (display "null input")
        (readlines (get-line (current-input-port)))
      )]
      [#t (begin
        (display "uh")
        (display (string-append line "\n"))
        (readlines (get-line (current-input-port)))
      )]
    )
  )
)

(define last-digit
  (lambda (str . digit)
    (cond
      ; No string left and no digit found
      [(and (null? str) (null? digit))
        (error "last-digit" "no digit found" str)]
      ; No string left and a digit was found
      [(null? str) (car digit)]
      ; Input was a digit, set digit when we recurse
      [(char-numeric? (car str))
        (last-digit (cdr str) (car str))]
      ; Input not a digit, no digit found already
      [(null? digit) (last-digit (cdr str))]
      ; Input not a digit, digit found
      [#t (last-digit (cdr str) (car digit))]


      ; TODO why doesn't this work? fails with 'attempt to apply non-procedure'
      ; I'm trying to create a list with the first element being the list of remaining characters,
      ; because digit is either null (an empty list) or a list with a digit
      ;[#t (begin
      ;  (display "iter1")
      ;  (apply last-digit (append (remainder) digit)))]
    )))

;;; Returns a pair of the first and last digit in the provided string
;;;
;;; Note: the last digit can also be the first digit
(define first-and-last-digit
  (lambda (str)
    (let [(inner (lambda (inner str)
      (cond
        [(char-numeric? (car str))
          (list (car str) (last-digit str))]
        ; Could have been an if
        [#t (inner inner (cdr str))]
      )))]
      ; Apply our implementation after turning the string to a list
      (inner inner (string->list str)))))

(define get-string-number
  (lambda (str)
    (string->number (apply string (first-and-last-digit str)))))

; TODO better idiom than (lambda (self x) ...)
(let [(readline (lambda (self x)
  (let [(line (get-line (current-input-port)))]
    (if (eof-object? line)
      x
      (begin
      (self self (+ x (get-string-number line))))
    ))))]
  (display (string-append (number->string (readline readline 0)) "\n")))
