(define tree-merge
  (lambda (tree1 tree2)
    (if (and (leaf? tree1) (leaf? tree2))
        (make-leaf (set-union (tree-labels tree1)(tree-labels tree2)))
        (cond ((leaf? tree1)
               (make-node (set-union (tree-labels tree1)(tree-labels tree2)) (node-left tree2) (node-right tree2)))
              ((leaf? tree2)
               (make-node (set-union (tree-labels tree1)(tree-labels tree2)) (node-left tree1) (node-right tree1)))
              (else (make-node (set-union (tree-labels tree1)(tree-labels tree2)) (tree-merge (node-left tree1) (node-left tree2)) (tree-merge (node-right tree1) (node-right tree2))))
        ))))