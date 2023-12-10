;;; Our bag of cubes. (R G B)
(define bag '(12 13 14))

;;; Checks if a provided round is valid. A round is a list of 3 integers, defined as rgb.
(define round-valid?
  (lambda (bag round)
    (cond
      ; Round fails if there are more red cubes in the round than the bag
      [(< (car bag) (car round)) #f]
      ; More green in round than bag
      [(< (cadr bag) (cadr round)) #f]
      ; More blue in round than bag
      [(< (caddr bag) (caddr round)) #f]
      ; No fails, so round is a success
      [#t #t]
    )))

;;; Checks if a game, which is a list of rounds, is valid. A game is a list of rounds
(define game-valid?
  (lambda (bag game)
    (let ((iter (lambda (self bag game)
          (cond
            ; All rounds possible, return true
            [(null? game) #t]
            ; Round impossible => game impossible
            [(not (round-valid? bag (car game))) #f]
            ; Round is possible and there are more rounds to check
            [#t (self self bag (cdr game))]
          ))))
      (iter iter bag game))))

;;; Finds the first space in a string, from the given index
(define find-first-char
  (lambda (string char index)
    (cond
      [(<= (string-length string) index) #f]
      [(char=? (string-ref string index) char) index]
      [#t (find-first-char string char (+ index 1))])))

;;; Parses the provided string for number of cubes and colour, and adds it to the hand to return
;;; Assumes the string is formatted correctly

; find space, string-> number from string before, and compare after to 
(define draw-cubes
  (lambda (string hand)
    (let ((space (find-first-char string #\space 0)))
      (let (
          (num (string->number (substring string 0 space)))
          (colour (substring string (+ space 1) (string-length string)))
        )
        (cond
          ; Who needs destructuring when you have cadddddr?
          [(string=? colour "red") (list (+ num (car hand)) (cadr hand) (caddr hand))]
          [(string=? colour "green") (list (car hand) (+ num (cadr hand)) (caddr hand))]
          [(string=? colour "blue") (list (car hand) (cadr hand) (+ num (caddr hand)))]
          [#t (error "draw-cubes" "invalid colour" string)])
      ))))

;;; Parses the game ID. Assumes a string at least " <id>:"
(define get-game-id
  (lambda (string)
    (let (
      (space (find-first-char string #\space 0))
      (colon (find-first-char string #\: 0)))
      (string->number (substring string (+ space 1) colon)))))

;;; Returns a parsed round from the provided string
;;;
;;; Example round: "8 green, 6 blue, 20 red"
;;; Can *only* pass one round. This does not handle semicolons
(define parse-round
  (lambda (round index . hand)
    (let (
        (comma (find-first-char round #\, index))
        (hand (if (null? hand) '(0 0 0) (car hand)))
      )
      (if comma
        ; Found a comma, we want to parse this draw and recurse to the next draw
        (let ((hand (draw-cubes (substring round index comma) hand)))
          (parse-round round (+ comma 2) hand))
        ; Last draw, parse to the end and return the final hand
        (draw-cubes (substring round index (string-length round)) hand)))))


;;; Returns a parsed game from the provided string
;;;
;;; Example game: "<round>; <round>; <round>"
(define parse-game
  (lambda (game index . rounds)
    (let [
      (semicolon (find-first-char game #\; index))
      (rounds (if (null? rounds) '() (car rounds)))
    ]
    (if semicolon
      ; Found a semicolon, parse round and recurse to the next round
      (let ((rounds (append rounds (list (parse-round (substring game index semicolon) 0)))))
        (parse-game game (+ semicolon 2) rounds))
      ; Last round, parse to the end and return all rounds
      (append rounds (list (parse-round (substring game index (string-length game)) 0)))
      ))))


;;; Parses a text line for game information.
;;;
;;; Input format: "Game <game_id>: x <colour>[, y <colour>[, z <colour>]]; [optionally more rounds]"
;;;
;;; Parameters: (line [game_id])
;;;
;;; Returns structure (game_id ((round 1) (round 2)...)))
(define parse-line
  (lambda (line)
    (let [
      (colon (find-first-char line #\: 0))
    ]
    (list (get-game-id line) (parse-game line (+ colon 2))))))

; Takes a port, not filename because I'm bad at structure
(define part1
  (lambda (input count)
    (if (not (port-eof? input))
      ; Parse game and check validity
      (let ((game-info (parse-line (get-line input))))
        (if (game-valid? bag (cadr game-info))
          ; Valid game, add its index to the count
          (part1 input (+ count (car game-info)))
          ; Invalid game, just keep going
          (part1 input count)))
      ; EOF, return count
      count)))

(begin
  (display "Game 1 results: ")
  (display
    (part1
      (open-file-input-port "inputs/day2" (file-options) (buffer-mode line) (native-transcoder))
      0))
  (display "\n"))