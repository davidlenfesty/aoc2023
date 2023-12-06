;;; Returns the "string-y" character present at the provided index of the provided string
;;; Null if nothing found
(define get-stringy-digit
  (lambda (str index)
    (let (
      (substitutions (list
        '("one" #\1)
        '("two" #\2)
        '("three" #\3)
        '("four" #\4)
        '("five" #\5)
        '("six" #\6)
        '("seven" #\7)
        '("eight" #\8)
        '("nine" #\9)
        '("zero" #\0)))
      (check-string (lambda (self subs)

        (cond
          ; string too short, recurse
          [(<(- (string-length str) index) (string-length (caar subs)))
            (if (null? (cdr subs))
              ; Nothing left to check
              '()
              ; More to check, recurse
              (self self (cdr subs)))]
          ; substring found
          [(string=? (caar subs) (substring str index (+ index (string-length (caar subs)))))
              (cadar subs)]
          ; End of substitutions, nothing found
          [(null? (cdr subs)) '()]
          ; Substring not found, recurse
          [#t (self self (cdr subs))]))))
      (check-string check-string substitutions))))

;;; Returns the corresponding digit character at the position in the string if it's a digit
(define digit-at-pos
  (lambda (str index)
    (cond
      ; Numeric character? return directly
      [(char-numeric? (string-ref str index))
        (string-ref str index)]
      ; Stringy-digit at position? return it
      [(not (null? (get-stringy-digit str index)))
        (get-stringy-digit str index)]
      ; No digit here, return null
      [#t '()])))

; This is invalid syntax in cond apparently. It's a form so let no worky
;[let ((c (string-ref str i))) [(char-numeric? c) c]]

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
        [(not (null? (digit-at-pos str i)))
          (last-digit str (+ i 1) (digit-at-pos str i))]
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
      ; Could use a let to not call digit-at-pos twice, but I don't care
      (let ((digit (digit-at-pos str i))) 
        (if (not (null? digit))
          (list digit (last-digit str i))
          (inner inner str (+ i 1))
          ))))]
      ; Apply our implementation
      (inner inner str 0))))

;;; Converts a pair of first and last digit into an integer
(define get-string-number
  (lambda (str)
    (string->number (begin (display (first-and-last-digit str))(apply string (first-and-last-digit str))))))

; TODO better idiom than (lambda (self x) ...)
(let [(readline (lambda (self x)
  (let [(line (get-line (current-input-port)))]
    (if (eof-object? line)
      x
      (begin
      (self self (+ x (get-string-number line))))
    ))))]
  (display (string-append "Part 2 answer: " (number->string (readline readline 0)) "\n")))
