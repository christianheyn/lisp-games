(load "./tdd.lisp")
(load "./chess.lisp")

(define-test field-index-spec
  :describe "Basic functions"
  :func field-index
  :tests (
    (:test "gives index from move (A 3)"
     :actual (field-index A 3)
     :expected 40)
    (:test "gives index from move (A 8)"
     :actual (field-index A 8)
     :expected 0)
    (:test "gives index from move (H 8)"
     :actual (field-index H 8)
     :expected 7)
    (:test "gives index from move (A 1)"
     :actual (field-index A 1)
     :expected 56)
    (:test "gives index from move (H 1)"
     :actual (field-index H 1)
     :expected 63)
    (:test "gives index from move (D 7)"
     :actual (field-index D 7)
     :expected 11)))
