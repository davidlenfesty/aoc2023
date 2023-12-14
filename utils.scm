;;; Open the input file for a given day
(define open-input
  (lambda (day)
    (let ((filename (string-append "inputs/day" (number->string day))))
      (open-file-input-port filename (file-options) (buffer-mode line) (native-transcoder)))))

; Copied from Day 3
(define contains?
  (lambda (list search-item)
    (if (null? list)
      #f
      (if (eq? (car list) search-item)
        #t
        (contains? (cdr list) search-item)))))

;;; Finds the first space in a string, from the given index
(define find-first-char
  (lambda (string char index)
    (cond
      [(<= (string-length string) index) #f]
      [(char=? (string-ref string index) char) index]
      [#t (find-first-char string char (+ index 1))])))
