
(import fmt-table)

(define labels
  '(f gggg))

(define cols
  '((123 456 77 54 1  5646547987 41 1)
    (111 22 3333 44 5 6 7 8888)))

(define aligns
  '(left center center center center center center right)) ; one alignment per column

(print (fmt-table labels cols))
