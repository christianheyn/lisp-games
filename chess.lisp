;; https://www.bbc.com/news/technology-31028787
;; https://www.chess.com/terms/chess-pieces
;; sbcl --script chess.lisp
(load "./utils")

(defun piece-letter (p)
  (let* ((sym-name (symbol-name p))
         (sym-type (subseq sym-name 0 1)))
    (if (equal sym-type "_") " " sym-type)))

(defun piece (p)
  "Gives the name of the Piece `p`"
  (let ((sym-type (piece-letter p)))
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

(defun show-field (f)
  (let* ((p (first f))
         (letter (piece-letter p))
         (b/w (color p))
         (color-bg-code (cond ((equal b/w 'White) "47")
                              ((equal b/w 'Black) "40")
                              (t "106")))
         (color-code (if (equal (color p) 'White) "30" "37"))
         (letter-cased (if (equal b/w 'White) letter (string-downcase letter))))
    (format nil
      (concatenate
        'string
        "~c[" color-bg-code "m"
        "~c[1m~c[" color-code "m"
        letter-cased "~c[0m") #\ESC #\ESC #\ESC #\ESC)))

(defun print-chess-board (the-game)
    (let ((output (apply #'concatenate 'string (mapcar #'show-field the-game))))
      (print output)))

(defun play-chess (&optional game-data)
  (let* ((the-game (if game-data game-data (start-game))))
    (print-chess-board the-game)))

(play-chess)

