;; https://www.bbc.com/news/technology-31028787
;; https://www.chess.com/terms/chess-pieces
;; sbcl --script chess.lisp
(load "./utils")

(defun piece (p)
  "Gives the name of the Piece `p`"
  (let* ((sym-name (symbol-name p))
         (sym-type (subseq sym-name 0 1)))
    (cond ((equal sym-type "R") 'Rook)
          ((equal sym-type "P") 'Pawn)
          ((equal sym-type "B") 'Bishop)
          ((equal sym-type "N") 'Knight)
          ((equal sym-type "Q") 'Queen)
          ((equal sym-type "K") 'King)
          ( t                   'Empty))))

(defun color (p)
  "Gives the color of the Piece `p`"
  (let* ((sym-name (symbol-name p))
         (sym-type (subseq sym-name 1 2)))
    (cond ((equal sym-type "B") 'Black)
          ((equal sym-type "W") 'White)
          ( t                   'Empty))))

(defun start-game ()
  (let ((field '(Rb Nb Bb Qb Kb Bb Nb Rb
                 Pb Pb Pb Pb Pb Pb Pb Pb
                 __ __ __ __ __ __ __ __
                 __ __ __ __ __ __ __ __
                 __ __ __ __ __ __ __ __
                 __ __ __ __ __ __ __ __
                 Pw Pw Pw Pw Pw Pw Pw Pw
                 Rw Nw Bw Qw Kw Bw Nw Rw)))
    (mapcar #'(lambda (x) (list x nil)) field)))

(defmacro field-index (x y) ;; gensym
  `((lambda (a b)
    (let ((index-x (position a '(A B C D E F G H)))
          (index-y (- 8 b)))
      (+ (* 8 index-y) index-x))) ',x ,y))

(defun b (str)
  (format nil (concatenate 'string "~c[40m" str "~c[0m") #\ESC #\ESC)
(defun w (str)
  (format nil (concatenate 'string "~c[47m" str "~c[0m") #\ESC #\ESC)

(defun rec (data row col)
  (let ((next-data (cdr data))
        (next-row (+ row 1))
        (next-col (if (= row 7) 0 (+ row 1)))
        (letter (write-to-string (first data))))
    (print (b letter))
    (rec next-data next-row next-col)))

(defun print-chess-board (the-game)
  (rec the-game 0 0))


(defun play-chess (&optional game-data)
  (let* ((the-game (if game-data game-data (start-game))))
    (print-chess-board the-game)))

(play-chess)

