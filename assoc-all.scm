(define assoc-all
  (lambda (x l)
    (cond ((null? l) '()) 
          ((equal? x (car (car l))) (cons (cdr (car l)) (assoc-all x (cdr l))))
          (else (assoc-all x (cdr l))))))
