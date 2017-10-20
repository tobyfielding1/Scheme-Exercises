;;; 26747987 tf3g14 tobias fielding
(define tf1
      (lambda (t2 pred2 c2)
        (let ((l (if (pred2 (tree-labels t2) (lambda (bool) bool)) (tree-labels t2) '())))
          (if (leaf? t2)
            (c2 (make-leaf l))
            (c2 (make-node l (tf1 (node-left t2) pred2 (lambda (tr)tr)) (tf1 (node-right t2) pred2 (lambda (tr)tr))))))))

    (define tf2
      (lambda (t2 pred2 c2)
        (let ((l2 (if (pred2 (tree-labels t2) (lambda (bool) bool)) '() (tree-labels t2))))
          (if (leaf? t2)
            (c2 (make-leaf l2))
            (c2 (make-node l2 (tf2 (node-left t2) pred2 (lambda (tr)tr)) (tf2 (node-right t2) pred2 (lambda (tr)tr))))))))

(define tree-filter-cps
  (lambda (pred t c)
      (c (tf1 t pred (lambda (r)r)) (tf2 t pred (lambda (r)r)))))