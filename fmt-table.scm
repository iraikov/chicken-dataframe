
(module fmt-table

        (fmt-table)
        
        (import scheme (chicken base) (chicken string) (chicken format)
                srfi-1 utf8 utf8-lolevel 
                fmt yasos yasos-collections)
  
;; See
;; https://en.wikipedia.org/wiki/Box-drawing_character
;; http://www.utf8-chartable.de/unicode-utf8-table.pl?start=9472&unicodeinhtml=dec
(define table-borders-dict
  '((space         . (#\space  (" " " " " ") (" " " " " ") (" " " " " ") (" " " " " ")))
    (space-single  . (#\x2500  ("│" " " "│") ("┌" "─" "┐") ("├" "─" "┤") ("└" "─" "┘")))
    (single        . (#\x2500  ("│" "│" "│") ("┌" "┬" "┐") ("├" "┼" "┤") ("└" "┴" "┘")))
    (rounded       . (#\x2500  ("│" "│" "│") ("╭" "┬" "╮") ("├" "┼" "┤") ("╰" "┴" "╯")))
    (double        . (#\═      ("║" "║" "║") ("╔" "╦" "╗") ("╠" "╬" "╣") ("╚" "╩" "╝")))))


(define (fmt-table labels cols #!key
                   (formatter ->string)
                   (border-style 'single)
                   (port #f)
                   (framed? #t)
                   (row-sep? #t)
                   (align 'left))

  (let* ([lens (map size cols)]
         [len1 (first lens)])
    (if (not (every (lambda (len)(= len len1)) (cdr lens)))
      (error 'fmt-table "All columns must have the same length")))

  
  (let
      ((cell-sizes
        (map max
             (map (lambda (col)
                    (reduce (lambda (x ax) (max (string-length (->string x)) ax)) 0 col))
                  cols)
             (map (lambda (label) (string-length (->string label)))
                  labels)))
       (style (alist-ref border-style table-borders-dict)))

    (if (and (list? align)
             (not (= (length align) (length cols))))
        (error "align does not have the same number of elements of the number of columns in the table"))

  (define-values (row-sep col-seps first-row-corners mid-row-corners last-row-corners)
    (apply values style))
  
  (define align-list
    (if (symbol? align)
        (list-tabulate (length cell-sizes) (lambda (i) align))
        align))

  (define (make-row-line row-corners)
    (let ((s (fmt-join
              (lambda (n) (dsp (make-utf8-string n row-sep)))
              cell-sizes
              (second row-corners))))
      (if framed? (cat (first row-corners) s (third row-corners))
          s)))

  (define (fmt-row row-data)
    (let ((s (fmt-join
              (lambda (elem)
                (let-values (((item size al) (apply values elem)))
                  (let ((str (formatter item)))
                    (case al
                      ((right) (pad/left size str))
                      ((left)  (pad size str))
                      (else (pad/both size str))))))
              (zip row-data cell-sizes align-list)
              (second col-seps))))
      (if framed? (cat (first col-seps) s (third col-seps)) s)))

  (let* ((rows-str (fmt-join fmt-row (generator->list (apply g-zip (map gen-elts cols))) "\n"))
         (labels-str (fmt-row labels)))

    (fmt port (cat (make-row-line first-row-corners) "\n"
                   labels-str "\n"
                   (make-row-line mid-row-corners) "\n"
                   rows-str "\n"
                   (make-row-line last-row-corners) "\n"))
    
    ))
  )
)

