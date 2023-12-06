;;; Finds the last digit in a string
(define last-digit
  (lambda (str i . digit)
    (let ((len (string-length str)))
      (cond
        ; No string left to parse
        [(eq? len i) (if (null? digit)
          ; digit not found 
          (error "last-digit" "no digit found" str)
          ; digit found
          (car digit)
        )]
        ; Input was a digit, set digit when we recurse
        [(char-numeric? (string-ref str i))
          (last-digit str (+ i 1) (string-ref str i))]
        ; Input not a digit, no digit found already
        [(null? digit) (last-digit str (+ i 1))]
        ; Input not a digit, digit found already
        [#t (last-digit str (+ i 1) (car digit))]


        ; TODO why doesn't this work? fails with 'attempt to apply non-procedure'
        ; I'm trying to create a list with the first element being the list of remaining characters,
        ; because digit is either null (an empty list) or a list with a digit
        ;[#t (begin
        ;  (display "iter1")
        ;  (apply last-digit (append (remainder) digit)))]
    )
    )))

;;; Returns a pair of the first and last digit in the provided string
;;;
;;; Note: the last digit can also be the first digit
(define first-and-last-digit
  (lambda (str)
    (let [(inner (lambda (inner str i)
      (if (char-numeric? (string-ref str i))
        (list (string-ref str i) (last-digit str i))
        (inner inner str (+ i 1))
      )))]
      ; Apply our implementation
      (inner inner str 0))))

;;; Converts a pair of first and last digit into an integer
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
  (display (string-append "Part 1 answer: " (number->string (readline readline 0)) "\n")))

;;; Returns the number at the "current" position in the "string" (list of characters), or false
(define get-number-at-position
  (lambda (str)
    (if (char-numeric? (car str))
      (car str)
      (let [
        (substitutions [
          ("one" #\1)
          ("two" #\2)
          ("three" #\3)
          ("four" #\4)
          ("five" #\5)
          ("six" #\6)
          ("seven" #\7)
          ("eight" #\8)
          ("nine" #\9)
          ("zero" #\0)])]

        (let [()])
    ))))
