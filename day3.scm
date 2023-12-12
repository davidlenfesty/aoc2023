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