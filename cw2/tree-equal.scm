(define tree-equal?
  (lambda (tree1 tree2)
    (if (and (set-equal? (tree-labels tree1) (tree-labels tree2)) (equal? (tree-tag tree1) (tree-tag tree2)))
        (if (node? tree1)
            (and (tree-equal? (node-left tree1) (node-left tree2)) (tree-equal? (node-right tree1) (node-right tree2)))
            #t)
        #f)))