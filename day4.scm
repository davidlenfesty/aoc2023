(load "utils.scm")

(define parse-number-with-leading-spaces
  (lambda (string index)
    (let [(char (string-ref string index))]
      (if (char=? char #\space)
        (parse-number-with-leading-spaces string (+ index 1))
        (string->number
          (let [
              (end-index (find-first-char string #\space index))
          ]
          (substring string index
            (if end-index
              end-index
              (string-length string)))))))))

; Parses the "number lists" defined in the problem.
;
; Assumes the string ends at the end of the last number, and formatting of all numbers is the same
(define parse-number-list
  (lambda (string index)
    (if (>= index (string-length string))
      '()
      (cons
        (parse-number-with-leading-spaces string index)
        (parse-number-list string (+ index 3))))))

; Returns the description of a card, (id (winning_numbers) (your_numbers))
(define parse-line
  (lambda (line)
    (let [
        (colon (find-first-char line #\: 0))
        (pipe (find-first-char line #\| 0))
      ]
      (let [
          (card
            (parse-number-with-leading-spaces line (- colon 3)))
          (winners
            (parse-number-list (substring line (+ colon 1) (- pipe 1)) 0))
          (yours
            (parse-number-list (substring line (+ pipe 1) (string-length line)) 0))
        ]
        (list card winners yours)))))