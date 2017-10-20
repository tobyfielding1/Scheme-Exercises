(define tree-map
  (lambda (f tree)
    (if (node? tree)
        (make-node (f (tree-labels tree)) (tree-map f (node-left tree)) (tree-map f (node-right tree)))
        (make-leaf (f (tree-labels tree))))))
    
(define remove-labels
  (lambda (labels tree)
    (if (node? tree)
        (make-node (set-minus (tree-labels tree) labels) (remove-labels labels (node-left tree)) (remove-labels labels (node-right tree)))
        (make-leaf (set-minus (tree-labels tree) labels)))))