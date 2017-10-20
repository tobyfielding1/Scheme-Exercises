
(define the-node-tag    (list 'node))
(define the-leaf-tag    (list 'leaf))

(define node?
  (lambda (tree)
    (and (pair? tree) (eq? (tree-tag tree) the-node-tag))))

(define leaf?
  (lambda (tree)
    (and (pair? tree) (eq? (tree-tag tree) the-leaf-tag))))

(define tree-tag    car)
(define tree-labels cadr)
(define node-left   caddr)
(define node-right  cadddr)

(define make-node
  (lambda (labels left right)
    (list the-node-tag labels left right)))

(define make-leaf
  (lambda (labels)
    (list the-leaf-tag labels)))

(define set-union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
	  ((memq (car set1) set2) (set-union (cdr set1) set2))
	  (else (cons (car set1) (set-union (cdr set1) set2))))))

(define set-minus
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member (car set1) set2) (set-minus (cdr set1) set2))
          (else (cons (car set1) (set-minus (cdr set1) set2))))))

(define set-equal?
  (lambda (set1 set2)
    (and (null? (set-minus set1 set2))
         (null? (set-minus set2 set1)))))

(define tree-equal?
  (lambda (tree1 tree2)
    (if (and (set-equal? (tree-labels tree1) (tree-labels tree2)) (equal? (tree-tag tree1) (tree-tag tree2)))
        (if (equal? (tree-tag tree1) the-node-tag)
            (and (tree-equal? (node-left tree1) (node-left tree2)) (tree-equal? (node-right tree1) (node-right tree2)))
            #t)
        #f)))

(define tree-map
  (lambda (f tree)
    (if (equal? (tree-tag tree) the-node-tag)
        (make-node (f (tree-labels tree)) (tree-map f (node-left tree)) (tree-map f (node-right tree)))
        (make-leaf (f (tree-labels tree))))))
    
(define remove-labels
  (lambda (labels tree)
    (if (equal? (tree-tag tree) the-node-tag)
        (make-node (set-minus (tree-labels tree) labels) (remove-labels labels (node-left tree)) (remove-labels labels (node-right tree)))
        (make-leaf (set-minus (tree-labels tree) labels)))))

(define tree-merge
  (lambda (tree1 tree2)
    (if (and (equal? (tree-tag tree1) the-leaf-tag) (equal? (tree-tag tree2) the-leaf-tag))
        (make-leaf (set-union (tree-labels tree1)(tree-labels tree2)))
        (cond ((equal? (tree-tag tree1) the-leaf-tag)
               (make-node (set-union (tree-labels tree1)(tree-labels tree2)) (node-left tree2) (node-right tree2)))
              ((equal? (tree-tag tree2) the-leaf-tag)
               (make-node (set-union (tree-labels tree1)(tree-labels tree2)) (node-left tree1) (node-right tree1)))
              (else (make-node (set-union (tree-labels tree1)(tree-labels tree2)) (tree-merge (node-left tree1) (node-left tree2)) (tree-merge (node-right tree1) (node-right tree2))))
        ))))

(define create-label
  (lambda (l)
    (string->symbol (symbol-append-reverse l))))

(define symbol-append-reverse
  (lambda (los)
    (if (null? los)
        ""
        (string-append (symbol-append-reverse (cdr los))
                       (symbol->string (car los))))))

(define tree-to-graph
  (lambda (tree)
    (create-graph tree "t")))
    
(define create-graph
  (lambda (tree path)
    (if (equal? (tree-tag tree) the-leaf-tag)
        (list(list 'vertex (string->symbol path) (tree-labels tree)))
        (append (list (list 'edge (string->symbol path) (string->symbol (string-append path "l")))
              (list 'edge (string->symbol path) (string->symbol (string-append path "r")))
              (list 'vertex (string->symbol path) (tree-labels tree)))
              (create-graph (node-left tree) (string-append path "l"))
              (create-graph (node-right tree) (string-append path "r"))))))

(define member-a
  (lambda (l cont)
    (cond ((null? l) (cont #f))
          ((member (car l) '(a aa aaa aaaa aaaaa)) (cont #t))
          (else (member-a (cdr l) cont)))))

 (define tf1
      (lambda (t2 pred2 c2 bool)
        (let ((l (if (eq? bool (pred2 (tree-labels t2) (lambda (bool) bool))) (tree-labels t2) '())))
          (if (leaf? t2)
            (c2 (make-leaf l))
            (c2 (make-node l (tf1 (node-left t2) pred2 (lambda (tr)tr) bool) (tf1 (node-right t2) pred2 (lambda (tr)tr) bool)))))))

(define tree-filter-cps
  (lambda (pred t c)
      (c (tf1 t pred (lambda (r)r) #t) (tf1 t pred (lambda (r)r) #f))))
                    
                        
(define separate-cps
  (lambda (pred l c)
    (if (null? l)
        (c '() '())
        (separate-cps pred
                      (cdr l)
                      (lambda (success fail)
                            (c (cons (car l) success) fail))))))

(define (atom? x) (not (or (pair? x) (null? x))))

(define atoms-cps
  (lambda (S c)
    (if (atom? S)
        (c 1)
        (atoms-cps (car S)
                   (lambda (x)
                     (atoms-cps (cdr S)
                                (lambda (y)
                                  (c (+ x y)))))))))