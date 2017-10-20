;;; 26747987 tf3g14 tobias fielding
(define tree-to-graph
  (lambda (tree)
    (create-graph tree "t")))
    
(define create-graph
  (lambda (tree path)
    (if (leaf? tree)
        (list(list 'vertex (string->symbol path) (tree-labels tree)))
        (append (list (list 'edge (string->symbol path) (string->symbol (string-append path "l")))
              (list 'edge (string->symbol path) (string->symbol (string-append path "r")))
              (list 'vertex (string->symbol path) (tree-labels tree)))
              (create-graph (node-left tree) (string-append path "l"))
              (create-graph (node-right tree) (string-append path "r"))))))