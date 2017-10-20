(define map-values
  (lambda (l f)
    (if (null? l) '()
      (cons (cons (car (car l)) (f (cdr (car l)))) (map-values (cdr l) f)))))